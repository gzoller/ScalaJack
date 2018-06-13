package co.blocke.scalajack
package typeadapter

object LongTypeAdapter extends TypeAdapter.=:=[Long] {

  override val deserializer: Deserializer[Long] = new LongDeserializer

  override val serializer: Serializer[Long] = new LongSerializer

  override def read(reader: Reader): Long =
    reader.readLong()

  override def write(value: Long, writer: Writer): Unit =
    writer.writeLong(value)

}
