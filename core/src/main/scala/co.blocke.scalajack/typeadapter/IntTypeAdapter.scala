package co.blocke.scalajack
package typeadapter

object IntTypeAdapter extends TypeAdapter.=:=[Int] {

  override val deserializer: Deserializer[Int] = new IntDeserializer

  override def read(reader: Reader): Int =
    reader.readInt()

  override def write(value: Int, writer: Writer): Unit =
    writer.writeInt(value)

}
