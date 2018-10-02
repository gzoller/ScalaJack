package co.blocke.scalajack
package typeadapter

import org.apache.commons.text.StringEscapeUtils.escapeJava

class StringSerializer extends Serializer[String] {

  override def serialize[J](tagged: TypeTagged[String])(implicit ops: JsonOps[J], guidance: SerializationGuidance): SerializationResult[J] =
    tagged match {
      case TypeTagged(null)  => SerializationSuccess(JsonNull())
      case TypeTagged(value) => SerializationSuccess(JsonString(escapeJava(value)))
    }

}
