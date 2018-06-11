package co.blocke.scalajack
package typeadapter

class CharDeserializer extends Deserializer[Char] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[Char] =
    json match {
      case JsonString(string) if string.length == 1 => DeserializationSuccess(TypeTagged(string.charAt(0)))
      case JsonString(string)                       => DeserializationFailure(path, DeserializationError.Malformed(s"Expected a JSON string of length 1, not ${string.length}"))
      case _                                        => DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON string of length 1"))
    }

}

class CharSerializer extends Serializer[Char] {

  override def serialize[J](tagged: TypeTagged[Char])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTaggedChar(charValue) => SerializationSuccess(JsonString("" + charValue))
      case TypeTagged(charValue)     => SerializationSuccess(JsonString("" + charValue))
    }

}

object CharTypeAdapter extends TypeAdapter.=:=[Char] with StringKind {

  override val deserializer: Deserializer[Char] = new CharDeserializer

  override val serializer: Serializer[Char] = new CharSerializer

  override def read(reader: Reader): Char = {
    reader.readString().head // TODO Ensure there is only one character
  }

  override def write(value: Char, writer: Writer): Unit =
    writer.writeChar(value)

}
