package co.blocke.scalajack
package typeadapter

object ByteTypeAdapter extends TypeAdapter.=:=[Byte] {

  override val deserializer: Deserializer[Byte] = new ByteDeserializer

  override val serializer: Serializer[Byte] = new ByteSerializer

  override def read(reader: Reader): Byte =
    reader.readByte()

  override def write(value: Byte, writer: Writer): Unit =
    writer.writeByte(value)

}
