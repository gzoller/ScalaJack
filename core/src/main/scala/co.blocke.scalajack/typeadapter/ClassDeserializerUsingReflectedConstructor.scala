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

  private def inferConcreteDeserializer[J](path: Path, json: J)(implicit ops: JsonOps[J]): Option[Deserializer[_ <: CC]] =
    if (typeMembers.isEmpty) {
      None
    } else {
      json match {
        case JsonObject(x) =>
          val fields = x.asInstanceOf[ops.ObjectFields]

          import scala.collection.mutable

          val setsOfTypeArgsByTypeParam = new mutable.HashMap[Symbol, mutable.HashSet[Type]]

          ops.foreachObjectField(fields, { (fieldName, fieldValueJson) =>
            for (typeMember <- typeMembersByName.get(fieldName)) {
              val actualType: Type = Try(typeDeserializer.deserialize(path \ fieldName, fieldValueJson)).map(_.get.get).getOrElse(typeMember.baseType)

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

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[CC] =
    json match {
      case JsonNull() =>
        DeserializationSuccess(nullTypeTagged)

      case JsonObject(x) =>
        try {
          val fields = x.asInstanceOf[ops.ObjectFields]

          val deserializationResultsByField: mutable.Map[FieldMember, DeserializationResult[Any]] = new mutable.HashMap[FieldMember, DeserializationResult[Any]]

          inferConcreteDeserializer(path, json) match {
            case Some(concreteDeserializer) =>
              concreteDeserializer.deserialize(path, json)

            case None =>
              ops.foreachObjectField(fields, { (fieldName, fieldValueJson) =>
                for (fieldMember <- fieldMembersByName.get(fieldName)) {
                  deserializationResultsByField(fieldMember) = fieldMember.deserializeValue[J](path \ fieldName, fieldValueJson)
                }
              })

              println(fieldMembers)
              println(fieldMembersByName)

              for (fieldMember <- fieldMembers if !deserializationResultsByField.contains(fieldMember)) {
                deserializationResultsByField(fieldMember) = fieldMember.deserializeValueFromNothing[J](path \ fieldMember.name)
              }

              if (deserializationResultsByField.exists(_._2.isFailure)) {
                // Uh-oh!
                DeserializationFailure(deserializationResultsByField.values.flatMap(_.errors).to[immutable.Seq])
              } else {
                val constructorArguments: Array[Any] = fieldMembers
                  .map { fieldMember =>
                    val DeserializationSuccess(TypeTagged(fieldValue)) = deserializationResultsByField(fieldMember)
                    fieldValue
                  }
                  .toArray

                val instanceOfCaseClass = constructorMirror.apply(constructorArguments: _*).asInstanceOf[CC]

                if (isSJCapture) {
                  val captured = mutable.Map.empty[String, Any]

                  ops.foreachObjectField(fields, { (name, valueJson) =>
                    fieldMembersByName.get(name) match {
                      case Some(_) =>
                      // do nothing... already built class
                      case None =>
                        captured.put(name, valueJson)
                    }
                  })

                  instanceOfCaseClass.asInstanceOf[SJCapture].captured = captured
                }

                DeserializationSuccess(TypeTagged[CC](instanceOfCaseClass, caseClassType))
              }
          }
        } catch {
          case NonFatal(e) =>
            DeserializationFailure(path, DeserializationError.ExceptionThrown(e))
        }
    }

}
