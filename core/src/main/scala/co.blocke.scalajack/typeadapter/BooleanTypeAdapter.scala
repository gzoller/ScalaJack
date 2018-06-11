package co.blocke.scalajack
package typeadapter

class BooleanDeserializer extends Deserializer[Boolean] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[Boolean] =
    json match {
      case JsonBoolean(booleanValue) => DeserializationSuccess(TypeTagged(booleanValue))
      case _                         => DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON boolean"))
    }

}

class BooleanSerializer extends Serializer[Boolean] {

  override def serialize[J](tagged: TypeTagged[Boolean])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTaggedBoolean(booleanValue) => SerializationSuccess(JsonBoolean(booleanValue))
      case TypeTagged(booleanValue)        => SerializationSuccess(JsonBoolean(booleanValue))
    }

}

object BooleanTypeAdapter extends TypeAdapter.=:=[Boolean] {

  override val deserializer: Deserializer[Boolean] = new BooleanDeserializer

  override val serializer: Serializer[Boolean] = new BooleanSerializer

  override def read(reader: Reader): Boolean =
    reader.peek match {
      case TokenType.False =>
        reader.read(expected = TokenType.False)
        false

      case TokenType.True =>
        reader.read(expected = TokenType.True)
        true

      case TokenType.Null =>
        throw new IllegalStateException("Expected token of type Boolean, not Null\n" + reader.showError())

      case actual =>
        reader.read()
        throw new IllegalStateException(s"Expected value token of type True or False, not $actual when reading Boolean value.  (Is your value wrapped in quotes or a number?)\n" + reader.showError())
    }

  override def write(value: Boolean, writer: Writer): Unit =
    if (value) {
      writer.writeTrue()
    } else {
      writer.writeFalse()
    }

}
