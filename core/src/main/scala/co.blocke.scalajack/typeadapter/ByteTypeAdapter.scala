package co.blocke.scalajack
package typeadapter

class ByteDeserializer extends Deserializer[Byte] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[Byte] =
    json match {
      case JsonLong(longValue) => DeserializationSuccess(TypeTagged(longValue.byteValue))
      case _                   => DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON number"))
    }

}

class ByteSerializer extends Serializer[Byte] {

  override def serialize[J](tagged: TypeTagged[Byte])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTaggedByte(byteValue) => SerializationSuccess(JsonLong(byteValue.longValue))
      case TypeTagged(byteValue)     => SerializationSuccess(JsonLong(byteValue.longValue))
    }

}

object ByteTypeAdapter extends TypeAdapter.=:=[Byte] {

  override val deserializer: Deserializer[Byte] = new ByteDeserializer

  override val serializer: Serializer[Byte] = new ByteSerializer

  override def read(reader: Reader): Byte =
    reader.readByte()

  override def write(value: Byte, writer: Writer): Unit =
    writer.writeByte(value)

}
