package co.blocke.series5
package typeadapter

object DoubleTypeAdapter extends SimpleTypeAdapter[Double] {

  override def read(reader: Reader): Double =
    reader.readDouble()

  override def write(value: Double, writer: Writer): Unit =
    writer.writeDouble(value)

}
