package co.blocke.scalajack
package typeadapter

object ShortTypeAdapter extends TypeAdapter.=:=[Short] {

  override val deserializer: Deserializer[Short] = new ShortDeserializer

  override val serializer: Serializer[Short] = new ShortSerializer

  override def read(reader: Reader): Short =
    reader.readShort()

  override def write(value: Short, writer: Writer): Unit =
    writer.writeShort(value)

}
