package co.blocke.scalajack
package typeadapter

class CharDeserializer extends Deserializer[Char] {

  self =>

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[Char] =
    json match {
      case JsonString(string) if string.length == 1 => DeserializationSuccess(TypeTagged(string.charAt(0)))
      case JsonString(string)                       => DeserializationFailure(path, DeserializationError.Malformed(s"Expected a JSON string of length 1, not ${string.length}"))
      case _                                        => DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON string of length 1", reportedBy = Some(self)))
    }

}
