package co.blocke.scalajack
package typeadapter

import co.blocke.scalajack.typeadapter.PlainClassTypeAdapter.PlainFieldMember

class PlainClassSerializer[C](members: List[PlainFieldMember[C]]) extends Serializer[C] {

  private type Member = PlainFieldMember[C]

  override def serialize[J](tagged: TypeTagged[C])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(null) =>
        SerializationSuccess(JsonNull())

      case TypeTagged(_) =>
        SerializationSuccess(JsonObject { appendField =>
          for (member <- members) {
            val memberName = member.fieldMapName.getOrElse(member.name)
            val memberValue = member.valueIn(tagged)

            val SerializationSuccess(memberValueJson) = member.serializeValue(memberValue)
            appendField(memberName, memberValueJson)
          }

          /*
          FIXME
          value match {
            case sjc: SJCapture =>
              sjc.captured.foreach {
                case (memberName, valueString) =>
                  memberNameTypeAdapter.write(memberName, writer)
                  writer.writeRawValue(valueString.asInstanceOf[String])
              }
            case _ =>
          }
           */

        })
    }

}
