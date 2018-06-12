package co.blocke.scalajack
package typeadapter

object FloatTypeAdapter extends TypeAdapter.=:=[Float] {

  override val deserializer: Deserializer[Float] = new FloatDeserializer

  override val serializer: Serializer[Float] = new FloatSerializer

  override def read(reader: Reader): Float =
    reader.readFloat()

  override def write(value: Float, writer: Writer): Unit =
    writer.writeFloat(value)

}
