package co.blocke.scalajack
package typeadapter

class StringDeserializer extends Deserializer[String] {

  self =>

  private val StringType: Type = typeOf[String]

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J], guidance: SerializationGuidance): DeserializationResult[String] =
    json match {
      case JsonNull()        => DeserializationSuccess(TypeTagged(null, StringType))
      case JsonString(value) => DeserializationSuccess(TypeTagged(value, StringType))
      case _                 => DeserializationFailure(path, DeserializationError.Unexpected("Expected a JSON string", reportedBy = self))
    }

}
