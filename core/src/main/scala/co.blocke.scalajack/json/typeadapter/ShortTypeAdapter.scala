package co.blocke.scalajack.json
package typeadapter

object ShortTypeAdapter extends SimpleTypeAdapter[Short] {

  override def read(reader: Reader): Short =
    reader.readShort()

  override def write(value: Short, writer: Writer): Unit =
    writer.writeShort(value)

}
