package co.blocke.scalajack
package typeadapter
package classes

import co.blocke.scalajack.model.ClassHelper.{ FieldMember, TypeMember }
import util.Path
import model._

import scala.collection.immutable.{ ListMap, Map }
import scala.collection.mutable.Builder

case class CaseClassTypeAdapter[T](
    className:         String,
    typeMembers:       List[ClassHelper.TypeMember[T]],
    fieldMembers:      ListMap[String, ClassHelper.ClassFieldMember[T, Any]],
    constructorMirror: MethodMirror,
    collectionName:    Option[String]                                        = None
) extends ClassHelper.ClassLikeTypeAdapter[T] {

  // Hook for subclasses (e.g. Mongo) do to anything needed to handle the db key field(s) as given by the @DBKey annotation
  protected def handleDBKeys[AST](fieldValues: Map[String, Any]): Map[String, Any] = fieldValues

  // OK, so all this hokem is to figure out what to do for embedded type member (i.e. externalized type hint feature).  Doesn't seem to be needed
  // for anything else.
  private def inferConcreteCaseClassTypeAdapter[AST](path: Path, ast: AST): Option[TypeAdapter[_ <: T]] =
    None

  /*
    if (typeMembers.isEmpty) {
      None
    } else {
      ast match {
        case IRObject(fields) =>

          import scala.collection.mutable

          val setsOfTypeArgsByTypeParam = new mutable.HashMap[Symbol, mutable.HashSet[Type]]

          fields.foreach {
            case (fieldName, fieldValueAst) =>
              for (typeMember <- typeMembersByName.get(fieldName)) {
                val actualType: Type = Try(typeTransceiver.read(path \ fieldName, fieldValueAst)).map(_.get).getOrElse(typeMember.baseType)

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

          val typeArgs = for (typeParam <- caseClassType.typeConstructor.typeParams) yield {
            val possibleTypeArgs = setsOfTypeArgsByTypeParam(typeParam).toList
            val typeArg :: Nil = possibleTypeArgs
            typeArg
          }

          val concreteType = appliedType(caseClassType.typeConstructor, typeArgs)

          if (concreteType =:= caseClassType) {
            // YAY! BUSINESS AS USUAL
            None
          } else {
            Some(context.irTransceiver(concreteType).asInstanceOf[IRTransceiver[CC]])
          }

        case _ =>
          None
      }
    }
    */

  def read[WIRE](path: Path, reader: Transceiver[WIRE], isMapKey: Boolean): T =
    reader.readObjectFields[T](path, fieldMembers, isMapKey) match {
      case null => null.asInstanceOf[T]
      case objectFieldResult: ObjectFieldResult => //(allFound: Boolean, args: Array[Any], flags: Array[Boolean]) =>
        if (!objectFieldResult.allThere) {
          val fieldArray = fieldMembers.values.toArray
          for (p <- 0 to fieldArray.size - 1) {
            if (!objectFieldResult.fieldSet(p)) {
              fieldArray(p).defaultValue.map(default => objectFieldResult.objectArgs(p) = default).orElse(
                if (fieldArray(p).isOptional)
                  None
                else
                  throw new ReadMissingError(path, s"Class $className missing field ${fieldArray(p).name}", List(className, fieldArray(p).name))
              )
            }
          }
        }
        constructorMirror.apply(objectFieldResult.objectArgs: _*).asInstanceOf[T]
    }

  def write[WIRE](t: T, writer: Transceiver[WIRE], out: Builder[Any, WIRE]): Unit = {
  }

}

/*
  ast match {
    case AstObject(astKV) =>
      val astFieldMap = handleDBKeys(path, astKV, fieldMembers).toMap
      val constructorArguments: Array[Any] = fieldMembers.map { constructorField =>
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
              for (fieldMember <- fieldMembersByName.get(fieldName)) {
                readResultsByField(fieldMember) = fieldMember.valueTypeAdapter.read(path \ fieldName, fieldValueAST)
              }
          }

          // Missing fields in AST... let's go deeper...
          for (fieldMember <- fieldMembers if !readResultsByField.contains(fieldMember))
            // Substitute any specified default values
            readResultsByField(fieldMember) = fieldMember.defaultValue.getOrElse {
              try {
                fieldMember.valueTypeAdapter.readFromNothing(path \ fieldMember.name)
              } catch {
                case t: Throwable => throw new ReadException(path \ fieldMember.name, "Missing required field " + fieldMember.name)
              }
            }

          // Now build the object by calling the constructor
          val constructorArguments: Array[Any] = fieldMembers.map(fieldMember => readResultsByField(fieldMember)).toArray
          val instanceOfCaseClass = constructorMirror.apply(constructorArguments: _*).asInstanceOf[T]

          /*
            if (isSJCapture) {
              val partitionedFields = ops.partitionObject(irobj, (name, _) => fieldMembersByName.keySet.contains(name))
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
