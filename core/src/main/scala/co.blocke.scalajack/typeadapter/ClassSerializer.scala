package co.blocke.scalajack
package typeadapter

import scala.reflect.runtime.universe.lub

class ClassSerializer[C](
    context:           Context,
    constructorMirror: MethodMirror,
    typeSerializer:    Serializer[Type],
    typeMembers:       List[CaseClassTypeAdapter.TypeMember[C]],
    fieldMembers:      List[ClassFieldMember[C]],
    isSJCapture:       Boolean)(implicit tt: TypeTag[C]) extends Serializer[C] {

  private val tpe: Type = tt.tpe
  private val TypeType: Type = typeOf[Type]

  override def serialize[J](tagged: TypeTagged[C])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(null) =>
        SerializationSuccess(JsonNull())

      case TypeTagged(value) =>
        SerializationSuccess(JsonObject { appendField =>
          if (typeMembers.nonEmpty) {
            import scala.collection.mutable

            val setsOfTypeArgsByTypeParam = new mutable.HashMap[Symbol, mutable.HashSet[Type]]

            for (fieldMember <- fieldMembers) {
              val TypeTagged(fieldValue) = fieldMember.valueIn(TypeTagged.inferFromRuntimeClass[C](value))
              val declaredFieldValueType = fieldMember.declaredValueType
              val actualFieldValueType = Reflection.inferTypeOf(fieldValue)(fieldMember.valueTypeTag)

              for (typeParam <- tpe.typeConstructor.typeParams) {
                for (typeMember <- typeMembers) {
                  val optionalTypeArg = Reflection.solveForNeedleAfterSubstitution(
                    haystackBeforeSubstitution = declaredFieldValueType,
                    haystackAfterSubstitution  = actualFieldValueType,
                    needleBeforeSubstitution   = typeParam.asType.toType)

                  for (typeArg <- optionalTypeArg) {
                    setsOfTypeArgsByTypeParam.getOrElseUpdate(typeParam, new mutable.HashSet[Type]) += typeArg
                  }
                }
              }
            }

            val substitutions: List[(Symbol, Type)] = (for ((typeParam, setOfTypes) <- setsOfTypeArgsByTypeParam) yield {
              typeParam -> lub(setOfTypes.toList)
            }).toList

            val substitutionMap = substitutions.toMap

            val typeParams = tpe.typeConstructor.typeParams
            val typeArgs = typeParams.map(typeParam => substitutionMap(typeParam))

            for (typeMember <- typeMembers) {
              val ttt = typeMember.typeSignature.substituteTypes(substitutions.map(_._1), substitutions.map(_._2))
              val SerializationSuccess(typeJson) = typeSerializer.serialize(TypeTagged(ttt, TypeType))
              appendField(typeMember.name, typeJson)
            }

            val newType = appliedType(tpe.typeConstructor, typeArgs)
            val newTypeAdapter = context.typeAdapter(newType).asInstanceOf[ClassLikeTypeAdapter[C]]

            for (member <- newTypeAdapter.fieldMembers) {
              val SerializationSuccess(memberValueJson) = member.serializeValue(member.valueIn(tagged))
              appendField(member.name, memberValueJson)
            }
          } else {
            for (member <- fieldMembers) {
              val taggedMemberValue = member.valueIn(tagged)
              // FIXME              val memberName = mappedFieldsByName.get(member.name).map(_.fieldMapName.get).getOrElse(member.name)
              val memberName = member.name

              val SerializationSuccess(memberValueJson) = member.serializeValue(taggedMemberValue)
              appendField(memberName, memberValueJson)
            }
          }

          value match {
            case sjc: SJCapture =>
              sjc.captured.foreach {
                case (memberName, valueString) =>
                // FIXME                  memberNameTypeAdapter.write(memberName, writer)
                // FIXME                  writer.writeRawValue(valueString.asInstanceOf[String])
              }
            case _ =>
          }
        })
    }

}
