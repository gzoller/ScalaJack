package co.blocke.scalajack
package typeadapter

import co.blocke.scalajack.typeadapter.PlainClassTypeAdapter.PlainFieldMember

class PlainClassSerializer[C](members: List[PlainFieldMember[C]], isSJCapture: Boolean) extends Serializer[C] {

  private type Member = PlainFieldMember[C]

  override def serialize[J](tagged: TypeTagged[C])(implicit ops: JsonOps[J], guidance: SerializationGuidance): SerializationResult[J] =
    tagged match {
      case TypeTagged(null) =>
        SerializationSuccess(JsonNull())

      case TypeTagged(obj) =>
        SerializationSuccess(JsonObject { appendField =>
          for (member <- members) {
            val memberName = member.name
            val memberValue = member.valueIn(tagged)

            member.serializeValue(memberValue) match {
              case SerializationSuccess(memberValueJson)                           => appendField(memberName, memberValueJson)
              case SerializationFailure(f) if f == Seq(SerializationError.Nothing) => // do nothing--ignore optional fields of value None
            }

            if (isSJCapture) {
              val sjc = obj.asInstanceOf[SJCapture]
              sjc.captured.map { cap =>
                cap.jsonOps.foreachObjectField(cap.capturedFields.asInstanceOf[cap.jsonOps.ObjectFields], { (memberName, memberValue) =>
                  appendField(memberName, JsonValue.transform[cap.JsonType, J](memberValue)(cap.jsonOps, ops))
                })
              }
            }

          }
        })
    }

}
