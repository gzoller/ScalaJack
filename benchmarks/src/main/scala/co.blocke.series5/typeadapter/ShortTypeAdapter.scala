package co.blocke.series5
package typeadapter

object ShortTypeAdapter extends SimpleTypeAdapter[Short] {

  override def read(reader: Reader): Short =
    reader.readShort()

  override def write(value: Short, writer: Writer): Unit =
    writer.writeShort(value)

}
