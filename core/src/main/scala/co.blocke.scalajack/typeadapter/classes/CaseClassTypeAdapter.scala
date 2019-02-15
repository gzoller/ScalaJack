package co.blocke.scalajack
package typeadapter
package classes

import util.Path
import model._

import scala.collection.immutable.{ ListMap, Map }
import scala.collection.mutable.Builder
import scala.util.Try
import scala.reflect.runtime.universe.{ ClassSymbol, MethodMirror, MethodSymbol, NoType, Symbol, TermName, Type, TypeTag, appliedType, typeOf }

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
  private def inferConcreteCaseClassTypeAdapter(): Option[TypeAdapter[_ <: T]] = {
    println("TMemb: " + typeMembersByName)
    None
    //    if (typeMembers.isEmpty) {
    //      None
    //    } else {
    //      import scala.collection.mutable
    //      val setsOfTypeArgsByTypeParam = new mutable.HashMap[Symbol, mutable.HashSet[Type]]
    //
    //      typeMembers.foreach { typeMember =>
    //        val fieldName = typeMember.name.toString

    /*
        for (oneMember <- typeMembersByName.get(fieldName)) {
          val actualType: Type = Try(typeTypeAdapter.read(path \ fieldName, reader)).getOrElse(typeMember.baseType)

          // Solve for each type parameter
          for (typeParam <- caseClassType.typeConstructor.typeParams) {
            val optionalTypeArg = Reflection.solveForNeedleAfterSubstitution(
              haystackBeforeSubstitution = typeMember.typeSignature,
              haystackAfterSubstitution  = actualType,
              needleBeforeSubstitution   = typeParam.asType.toType)

            for (typeArg <- optionalTypeArg) {
              setsOfTypeArgsByTypeParam.getOrElseUpdate(typeParam, new mutable.HashSet[Type]) += typeArg
            }
          }
        }
      }
        */

    //          val typeArgs = for (typeParam <- caseClassType.typeConstructor.typeParams) yield {
    //            val possibleTypeArgs = setsOfTypeArgsByTypeParam(typeParam).toList
    //            val typeArg :: Nil = possibleTypeArgs
    //            typeArg
    //          }

    //          val concreteType = appliedType(caseClassType.typeConstructor, typeArgs)
    //
    //          if (concreteType =:= tt.tpe)
    //            None // YAY! BUSINESS AS USUAL
    //          else
    //            Some(context.typeAdapter(concreteType).asInstanceOf[TypeAdapter[_ <: T]])

    //      None
    //    }
  }

  def read[WIRE](path: Path, reader: Transceiver[WIRE]): T =
    reader.readObjectFields[T](path, isSJCapture, fieldMembersByName) match {
      case null => null.asInstanceOf[T]
      case objectFieldResult: ObjectFieldResult =>
        inferConcreteCaseClassTypeAdapter()
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
