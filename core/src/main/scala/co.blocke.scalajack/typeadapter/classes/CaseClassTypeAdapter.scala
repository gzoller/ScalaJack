package co.blocke.scalajack
package typeadapter
package classes

import util.Path
import model._

import scala.collection.immutable.{ ListMap, Map }
import scala.collection.mutable.Builder
import scala.reflect.runtime.universe.{ MethodMirror, Type, TypeTag }

case class CaseClassTypeAdapter[T](
    className:          String,
    typeMembersByName:  Map[String, ClassHelper.TypeMember[T]],
    fieldMembersByName: ListMap[String, ClassHelper.ClassFieldMember[T, Any]],
    typeTypeAdapter:    TypeAdapter[Type],
    constructorMirror:  MethodMirror,
    isSJCapture:        Boolean,
    collectionName:     Option[String]                                        = None)(implicit context: Context, tt: TypeTag[T]) extends ClassHelper.ClassLikeTypeAdapter[T] {

  // Hook for subclasses (e.g. Mongo) do to anything needed to handle the db key field(s) as given by the @DBKey annotation
  protected def handleDBKeys[AST](fieldValues: Map[String, Any]): Map[String, Any] = fieldValues

  // OK, so all this hokem is to figure out what to do for embedded type member (i.e. externalized type hint feature).  Doesn't seem to be needed
  // for anything else.
  //  private def inferConcreteCaseClassTypeAdapter[WIRE](path: Path, reader: Transceiver[WIRE]): Option[TypeAdapter[_ <: T]] = {
  private def inferConcreteCaseClassTypeAdapter[WIRE](path: Path, reader: Transceiver[WIRE]): ListMap[String, ClassHelper.ClassFieldMember[T, Any]] = {
    if (typeMembersByName.isEmpty) {
      fieldMembersByName
    } else {
      // If type members are defined --> externalized trait concrete type
      // Create a mapping of type label, e.g. 'T', to TypeMember where we've resolved the type member's value into a Type
      val concrete = typeMembersByName.map { case (name, tm) => (tm.typeSignature.toString, tm.copy(runtimeConcreteType = reader.lookAheadForTypeHint(name, (s: String) => typeTypeAdapter.read(path, reader)))) }

      // Now buzz through known field members and replaces all the 'T' type with the concrete type and insert the correct concrete TypeAdapter
      fieldMembersByName.map {
        case (name, field) =>
          val findConcrete = concrete.get(field.declaredValueType.toString) match {
            case Some(c) => field.copy(
              valueTypeAdapter  = context.typeAdapter(c.runtimeConcreteType.get),
              declaredValueType = c.runtimeConcreteType.get
            ).asInstanceOf[ClassHelper.ClassFieldMember[T, Any]]
            case None => field
          }
          (name, findConcrete) //.map(actual => field.copy(declaredValueType = actual.runtimeConcreteType.get)).getOrElse(field))
      }
    }
  }

  def read[WIRE](path: Path, reader: Transceiver[WIRE]): T = {
    val fixed = inferConcreteCaseClassTypeAdapter(path, reader)
    reader.readObjectFields[T](path, isSJCapture, fixed) match {
      case null => null.asInstanceOf[T]
      case objectFieldResult: ObjectFieldResult =>
        if (!objectFieldResult.allThere) {
          val fieldArray = fieldMembersByName.values.toArray
          for (p <- 0 to fieldArray.size - 1) {
            if (!objectFieldResult.fieldSet(p)) {
              fieldArray(p).defaultValue.map(default => objectFieldResult.objectArgs(p) = default).orElse(
                if (fieldArray(p).isOptional)
                  None
                else
                  throw new ReadMissingError(path, s"Class $className missing field ${fieldArray(p).name}\n" + reader.showError(), List(className, fieldArray(p).name)))
            }
          }
        }
        val asBuilt = constructorMirror.apply(objectFieldResult.objectArgs: _*).asInstanceOf[T]
        if (isSJCapture)
          asBuilt.asInstanceOf[SJCapture].captured = objectFieldResult.captured.get
        asBuilt
    }
  }

  def write[WIRE](t: T, writer: Transceiver[WIRE], out: Builder[Any, WIRE], isMapKey: Boolean): Unit =
    writer.writeObject(t, fieldMembersByName, out)

  // Used by AnyTypeAdapter to insert type hint (not normally needed) into output so object
  // may be reconsituted on read
  def writeWithHint[WIRE](t: T, writer: Transceiver[WIRE], out: Builder[Any, WIRE], isMapKey: Boolean): Unit = {
    val hintValue = t.getClass.getName
    val hintLabel = writer.jackFlavor.getHintLabelFor(tt.tpe)
    val extra = List((hintLabel, ClassHelper.ExtraFieldValue(hintValue, writer.jackFlavor.stringTypeAdapter)))
    writer.writeObject(t, fieldMembersByName, out, extra)
  }
}

/*
  ast match {
    case AstObject(astKV) =>
      val astFieldMap = handleDBKeys(path, astKV, fieldMembersByName).toMap
      val constructorArguments: Array[Any] = fieldMembersByName.map { constructorField =>
        astFieldMap.get(constructorField.name.toString)
          .map(fieldAst => constructorField.valueTypeAdapter.read(path \ constructorField.name.toString, fieldAst))
          .getOrElse(if (constructorField.isOptional) None else constructorField.defaultValue.getOrElse(throw new Exception("Boom")))
      }
      constructorMirror.apply(constructorArguments: _*).asInstanceOf[T]
      */

/*
      val readResultsByField: mutable.Map[ClassHelper.FieldMember[T], Any] = new mutable.HashMap[ClassHelper.FieldMember[T], Any]

      inferConcreteCaseClassTypeAdapter(path, postDBKey) match {
        case Some(inferredCaseClassTypeAdapter) =>
          inferredCaseClassTypeAdapter.read(path, ast)

        case None =>
          val irobj @ AstObject(fields) = postDBKey

          // First the easy stuff... pick up the known fields we find in the AST
          fields.foreach {
            case (fieldName, fieldValueAST) =>
              for (fieldMember <- fieldMembersByNameByName.get(fieldName)) {
                readResultsByField(fieldMember) = fieldMember.valueTypeAdapter.read(path \ fieldName, fieldValueAST)
              }
          }

          // Missing fields in AST... let's go deeper...
          for (fieldMember <- fieldMembersByName if !readResultsByField.contains(fieldMember))
            // Substitute any specified default values
            readResultsByField(fieldMember) = fieldMember.defaultValue.getOrElse {
              try {
                fieldMember.valueTypeAdapter.readFromNothing(path \ fieldMember.name)
              } catch {
                case t: Throwable => throw new ReadException(path \ fieldMember.name, "Missing required field " + fieldMember.name)
              }
            }

          // Now build the object by calling the constructor
          val constructorArguments: Array[Any] = fieldMembersByName.map(fieldMember => readResultsByField(fieldMember)).toArray
          val instanceOfCaseClass = constructorMirror.apply(constructorArguments: _*).asInstanceOf[T]

          /*
            if (isSJCapture) {
              val partitionedFields = ops.partitionObject(irobj, (name, _) => fieldMembersByNameByName.keySet.contains(name))
              val captured = partitionedFields._2.asInstanceOf[ops.ObjectType] // fields not in class we need to save

              val aux = ops.asInstanceOf[Ops.Aux[IR, WIRE, ops.ObjectType]]
              instanceOfCaseClass.asInstanceOf[SJCapture].captured = Some(IRAndOps[IR, WIRE, ops.ObjectType](captured)(aux))
            }
            */
          instanceOfCaseClass
        }

    case AstNull() =>
      null.asInstanceOf[T]

    //      case AstString(s) if (g.isMapKey) =>
    //        this.read(path, g.flavor.parse(s.asInstanceOf[g.flavor.WIRE_TYPE]).asInstanceOf[AST])(ops, g.copy(isMapKey = false))

    case _ =>
      throw new ReadException(path, s"Expected a JSON object, not $ast")
  }

  //  override def write[AST](t: T)(implicit ops: Ops[AST], g: SerializationGuidance): AST =
  //    throw new Exception("Write BOOM!")
}
  */
