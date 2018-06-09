package co.blocke.scalajack
package typeadapter

object DoubleTypeAdapter extends TypeAdapter.=:=[Double] {

  override def read(reader: Reader): Double =
    reader.readDouble()

  override def write(value: Double, writer: Writer): Unit =
    writer.writeDouble(value)

}
