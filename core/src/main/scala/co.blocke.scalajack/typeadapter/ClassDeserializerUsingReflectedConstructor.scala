package co.blocke.scalajack
package typeadapter

import scala.collection.{ immutable, mutable }
import scala.util.Try
import scala.util.control.NonFatal

class ClassDeserializerUsingReflectedConstructor[CC](
    context:           Context,
    constructorMirror: MethodMirror,
    typeDeserializer:  Deserializer[Type],
    typeMembers:       List[CaseClassTypeAdapter.TypeMember[CC]],
    fieldMembers:      List[ClassFieldMember[CC]],
    isSJCapture:       Boolean)(implicit tt: TypeTag[CC]) extends Deserializer[CC] {

  private type TypeMember = CaseClassTypeAdapter.TypeMember[CC]
  private type FieldMember = ClassFieldMember[CC]

  private val caseClassType: Type = tt.tpe
  private val nullTypeTagged: TypeTagged[CC] = TypeTagged[CC](null.asInstanceOf[CC], caseClassType)
  private val typeMembersByName: Map[String, TypeMember] = typeMembers.map(typeMember => typeMember.name -> typeMember).toMap
  private val fieldMembersByName: Map[String, FieldMember] = fieldMembers.map(fieldMember => fieldMember.name -> fieldMember).toMap

  // Hook for subclasses (e.g. Mongo) do to anything needed to handle the db key field(s) as given by the @DBKey annotation
  protected def handleDBKeys[AST, S](path: Path, ast: AST, members: List[ClassFieldMember[CC]])(implicit ops: AstOps[AST, S]): Either[DeserializationFailure, AST] = Right(ast)

  private def inferConcreteDeserializer[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): Option[Deserializer[_ <: CC]] =
    if (typeMembers.isEmpty) {
      None
    } else {
      ast match {
        case AstObject(x) =>
          val fields = x.asInstanceOf[ops.ObjectFields]

          import scala.collection.mutable

          val setsOfTypeArgsByTypeParam = new mutable.HashMap[Symbol, mutable.HashSet[Type]]

          ops.foreachObjectField(fields, { (fieldName, fieldValueAst) =>
            for (typeMember <- typeMembersByName.get(fieldName)) {
              val actualType: Type = Try(typeDeserializer.deserialize(path \ fieldName, fieldValueAst)).map(_.get.get).getOrElse(typeMember.baseType)

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
          })

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
            Some(context.deserializer(concreteType).asInstanceOf[Deserializer[_ <: CC]])
          }

        case _ =>
          None
      }
    }

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[CC] = {
    ast match {
      case AstNull() =>
        DeserializationSuccess(nullTypeTagged)

      case AstObject(_) =>
        try {
          handleDBKeys(path, ast, fieldMembers) match {
            case Left(fail) => fail
            case Right(postDBKey) =>
              val deserializationResultsByField: mutable.Map[FieldMember, DeserializationResult[Any]] = new mutable.HashMap[FieldMember, DeserializationResult[Any]]

              inferConcreteDeserializer(path, postDBKey) match {
                case Some(concreteDeserializer) =>
                  concreteDeserializer.deserialize(path, ast)

                case None =>
                  val fields = AstObject.unapply(postDBKey).get.asInstanceOf[ops.ObjectFields]
                  ops.foreachObjectField(fields, { (fieldName, fieldValueAst) =>
                    for (fieldMember <- fieldMembersByName.get(fieldName)) {
                      deserializationResultsByField(fieldMember) = fieldMember.deserializeValue[AST, S](path \ fieldName, fieldValueAst)
                    }
                  })

                  //                            println(fieldMembers)
                  //                            println(fieldMembersByName)

                  // Missing fields in JSON... let's go deeper...
                  for (fieldMember <- fieldMembers if !deserializationResultsByField.contains(fieldMember)) {
                    // Substitute any specified default values
                    deserializationResultsByField(fieldMember) = if (fieldMember.defaultValue.isDefined)
                      DeserializationSuccess(TypeTagged(fieldMember.defaultValue.get, fieldMember.declaredValueType))
                    else
                      fieldMember.deserializeValueFromNothing[AST, S](path \ fieldMember.name)
                  }

                  if (deserializationResultsByField.exists(_._2.isFailure))
                    // Uh-oh!  One or more fields *still* didn't deserialize (after default value substitution).
                    DeserializationFailure(deserializationResultsByField.values.flatMap(_.errors).to[immutable.Seq])
                  else {
                    val constructorArguments: Array[Any] = fieldMembers
                      .map { fieldMember =>
                        val DeserializationSuccess(TypeTagged(fieldValue)) = deserializationResultsByField(fieldMember)
                        fieldValue
                      }
                      .toArray

                    val instanceOfCaseClass = constructorMirror.apply(constructorArguments: _*).asInstanceOf[CC]

                    if (isSJCapture) {
                      val partitionedFields = ops.partitionObjectFields(fields, fieldMembersByName.keySet.toList)
                      val captured = partitionedFields._2
                      import AstOps._
                      implicit val aux: Aux[AST, ops.ObjectFields, S] = ops.asInstanceOf[Aux[AST, ops.ObjectFields, S]]
                      instanceOfCaseClass.asInstanceOf[SJCapture].captured = Some(AstAndOps(captured))
                    }
                    DeserializationSuccess(TypeTagged[CC](instanceOfCaseClass, caseClassType))
                  }
              }
          }
        } catch {
          case NonFatal(e) =>
            DeserializationFailure(path, DeserializationError.ExceptionThrown(e))
        }

      case AstString(s) if (guidance.isMapKey) =>
        val deserializer = context.typeAdapterOf[CC].deserializer
        deserializer.deserialize(Path.Root, ops.parse(s.asInstanceOf[S]))

      case _ =>
        DeserializationFailure(path, DeserializationError.Unexpected(s"Expected a JSON object, not $ast", this))
    }
  }

}
