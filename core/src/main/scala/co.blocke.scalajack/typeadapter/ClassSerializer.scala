package co.blocke.scalajack
package typeadapter

import scala.reflect.runtime.universe.lub
import scala.collection.immutable

class ClassSerializer[C](
    context:           Context,
    constructorMirror: MethodMirror,
    typeSerializer:    Serializer[Type],
    typeMembers:       List[CaseClassTypeAdapter.TypeMember[C]],
    fieldMembers:      List[ClassFieldMember[C]],
    isSJCapture:       Boolean)(implicit tt: TypeTag[C]) extends Serializer[C] {

  private val tpe: Type = tt.tpe
  private val TypeType: Type = typeOf[Type]

  override def serialize[J](tagged: TypeTagged[C])(implicit ops: JsonOps[J], guidance: SerializationGuidance): SerializationResult[J] =
    tagged match {
      case TypeTagged(null) =>
        SerializationSuccess(JsonNull())

      case TypeTagged(value) =>
        val errorsBuilder = immutable.Seq.newBuilder[SerializationError]

        val json = JsonObject[J] { appendField =>
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
              val typeSerializationResult = typeSerializer.serialize(TypeTagged(ttt, TypeType))
              typeSerializationResult match {
                case SerializationSuccess(typeJson) =>
                  appendField(typeMember.name, typeJson)
                case SerializationFailure(typeErrors) =>
                  errorsBuilder ++= typeErrors
              }
            }

            val newType = appliedType(tpe.typeConstructor, typeArgs)
            import collection.JavaConverters._
            val newTypeAdapter = context.typeAdapter(newType).asInstanceOf[ClassLikeTypeAdapter[C]]

            for (member <- newTypeAdapter.fieldMembers) {
              val valueSerializationResult = member.serializeValue(member.valueIn(tagged))
              valueSerializationResult match {
                case SerializationSuccess(valueJson) =>
                  appendField(member.name, valueJson)
                case SerializationFailure(valueErrors) =>
                  errorsBuilder ++= valueErrors
              }
            }
          } else {
            for (member <- fieldMembers) {
              val taggedMemberValue = member.valueIn(tagged)
              // FIXME              val memberName = mappedFieldsByName.get(member.name).map(_.fieldMapName.get).getOrElse(member.name)
              val memberName = member.name

              val valueSerializationResult = member.serializeValue(taggedMemberValue)

              valueSerializationResult match {
                case SerializationSuccess(memberValueJson) =>
                  appendField(memberName, memberValueJson)
                case failure @ SerializationFailure(_) if failure.isNothing =>
                // Nothing to do here
                case SerializationFailure(valueErrors) =>
                  errorsBuilder ++= valueErrors
              }
            }
          }

          value match {
            case sjc: SJCapture =>
              val captured = sjc.captured
              import captured.{ jsonValue, jsonOps }
              jsonValue match {
                case JsonObject(x) =>
                  val fields = x.asInstanceOf[jsonOps.ObjectFields]
                  jsonOps.foreachObjectField(fields, { (memberName, memberValue) =>
                    appendField(memberName, JsonValue.transform[captured.JsonValue, J](memberValue))
                  })
              }
            //              jsonValue.foreach {
            //                case (memberName, valueString) =>
            // FIXME                  memberNameTypeAdapter.write(memberName, writer)
            // FIXME                  writer.writeRawValue(valueString.asInstanceOf[String])
            //              }
            case _ =>
          }
        }

        val errors = errorsBuilder.result()

        if (errors.nonEmpty) {
          SerializationFailure(errors)
        } else {
          SerializationSuccess(json)
        }
    }

}
