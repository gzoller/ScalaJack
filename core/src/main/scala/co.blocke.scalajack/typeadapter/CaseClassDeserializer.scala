package co.blocke.scalajack
package typeadapter

import scala.collection.mutable
import scala.util.Try

class CaseClassDeserializer[CC](
    context:               Context,
    tpe:                   Type,
    constructorMirror:     MethodMirror,
    memberNameTypeAdapter: TypeAdapter[MemberName],
    typeTypeAdapter:       TypeAdapter[Type],
    typeMembers:           List[CaseClassTypeAdapter.TypeMember[CC]],
    fieldMembers:          List[ClassFieldMember[CC]],
    isSJCapture:           Boolean,
    collectionName:        Option[String]                            = None)(implicit tt: TypeTag[CC]) extends Deserializer[CC] {

  private type TypeMember = CaseClassTypeAdapter.TypeMember[CC]
  private type FieldMember = ClassFieldMember[CC]

  private val caseClassType: Type = tt.tpe
  private val nullTypeTagged: TypeTagged[CC] = TypeTagged[CC](null.asInstanceOf[CC], caseClassType)
  private val numberOfFieldMembers = fieldMembers.size
  private val typeMembersByName: Map[String, TypeMember] = typeMembers.map(typeMember => typeMember.name -> typeMember).toMap
  private val fieldMembersByScalaFieldName: Map[String, FieldMember] = fieldMembers.map(fieldMember => fieldMember.name -> fieldMember).toMap
  private val fieldMembersByJsonFieldName: Map[String, FieldMember] = fieldMembers.map(fieldMember => fieldMember.fieldMapName.getOrElse(fieldMember.name) -> fieldMember).toMap

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[CC] =
    json match {
      case JsonNull() =>
        DeserializationSuccess(nullTypeTagged)

      case JsonObject(x) =>
        val fields = x.asInstanceOf[ops.ObjectFields]

        val deserializationResults: mutable.Map[FieldMember, DeserializationResult[Any]] = new mutable.HashMap[FieldMember, DeserializationResult[Any]]
        var foundCount = 0

        if (typeMembers.nonEmpty) {
          import scala.collection.mutable

          val setsOfTypeArgsByTypeParam = new mutable.HashMap[Symbol, mutable.HashSet[Type]]

          ops.foreachObjectField(fields, { (fieldName, fieldValueJson) =>
            for (typeMember <- typeMembersByName.get(fieldName)) {
              val actualType: Type = Try(typeTypeAdapter.deserializer.deserialize(path \ fieldName, fieldValueJson)).map(_.get.get).getOrElse(typeMember.baseType)

              // Solve for each type parameter
              for (typeParam <- tpe.typeConstructor.typeParams) {
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

          val typeArgs = for (typeParam <- tpe.typeConstructor.typeParams) yield {
            val possibleTypeArgs = setsOfTypeArgsByTypeParam(typeParam).toList
            val typeArg :: Nil = possibleTypeArgs
            typeArg
          }

          val actualType = appliedType(tpe.typeConstructor, typeArgs)

          if (actualType =:= tpe) {
            // YAY! BUSINESS AS USUAL
          } else {
            val actualTypeAdapter = context.typeAdapter(actualType)
            return actualTypeAdapter.deserializer.deserialize(path, json).asInstanceOf[DeserializationResult[CC]]
          }
        }

        ops.foreachObjectField(fields, { (fieldName, fieldValueJson) =>
          for (fieldMember <- fieldMembersByJsonFieldName.get(fieldName)) {
            deserializationResults(fieldMember) = fieldMember.deserializeValue[J](path \ fieldName, fieldValueJson)
            foundCount += 1
          }
        })

        for (fieldMember <- fieldMembers if !deserializationResults.contains(fieldMember)) {
          deserializationResults(fieldMember) = fieldMember.deserializeValueFromNothing[J](path \ fieldMember.fieldMapName.getOrElse(fieldMember.name))
        }

        val constructorArguments: Array[Any] = fieldMembers.map(fieldMember => deserializationResults(fieldMember).get.get).toArray

        val instanceOfCaseClass = constructorMirror.apply(constructorArguments: _*).asInstanceOf[CC]

        if (isSJCapture) {
          val captured = mutable.Map.empty[String, Any]

          ops.foreachObjectField(fields, { (name, valueJson) =>
            fieldMembersByJsonFieldName.get(name) match {
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
