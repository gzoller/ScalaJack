package co.blocke.scalajack
package typeadapter

class StringDeserializer extends Deserializer[String] {

  self =>

  private val StringType: Type = typeOf[String]

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[String] =
    json match {
      case JsonNull()        => DeserializationSuccess(TypeTagged(null, StringType))
      case JsonString(value) => DeserializationSuccess(TypeTagged(value, StringType))
      case _                 => DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON string", reportedBy = Some(self)))
    }

}
