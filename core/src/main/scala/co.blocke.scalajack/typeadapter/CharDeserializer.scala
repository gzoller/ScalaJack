package co.blocke.scalajack
package typeadapter

class CharDeserializer extends Deserializer[Char] {

  self =>

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J], guidance: SerializationGuidance): DeserializationResult[Char] =
    json match {
      case JsonString(string) if string.length == 1 => DeserializationSuccess(TypeTagged(string.charAt(0)))
      case JsonString(string)                       => DeserializationFailure(path, DeserializationError.Malformed(s"Expected a JSON string of length 1, not ${string.length}", reportedBy = self))
      case _                                        => DeserializationFailure(path, DeserializationError.Unexpected("Expected a JSON string of length 1", reportedBy = self))
    }

}
