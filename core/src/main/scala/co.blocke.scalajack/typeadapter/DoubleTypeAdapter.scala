package co.blocke.scalajack
package typeadapter

object DoubleTypeAdapter extends TypeAdapter.=:=[Double] {

  override val deserializer: Deserializer[Double] = new DoubleDeserializer

  override val serializer: Serializer[Double] = new DoubleSerializer

  override def read(reader: Reader): Double =
    reader.readDouble()

  override def write(value: Double, writer: Writer): Unit =
    writer.writeDouble(value)

}
