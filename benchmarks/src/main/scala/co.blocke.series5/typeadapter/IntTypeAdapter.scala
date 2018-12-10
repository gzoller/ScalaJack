package co.blocke.series5
package typeadapter

object IntTypeAdapter extends SimpleTypeAdapter[Int] {

  override def read(reader: Reader): Int =
    reader.readInt()

  override def write(value: Int, writer: Writer): Unit =
    writer.writeInt(value)

}
