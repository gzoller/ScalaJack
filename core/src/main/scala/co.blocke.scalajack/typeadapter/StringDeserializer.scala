package co.blocke.scalajack
package typeadapter

//import org.apache.commons.text.StringEscapeUtils.escapeJava

class StringDeserializer extends Deserializer[String] {

  self =>

  private val StringType: Type = typeOf[String]

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J], guidance: SerializationGuidance): DeserializationResult[String] =
    json match {
      case JsonNull() => DeserializationSuccess(TypeTagged(null, StringType))
      case JsonString(value) =>
        println("A String: " + value)
        DeserializationSuccess(TypeTagged(value, StringType))
      case _ => DeserializationFailure(path, DeserializationError.Unexpected("Expected a JSON string", reportedBy = self))
    }

}
