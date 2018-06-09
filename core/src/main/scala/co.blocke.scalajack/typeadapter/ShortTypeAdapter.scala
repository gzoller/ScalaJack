package co.blocke.scalajack
package typeadapter

object ShortTypeAdapter extends TypeAdapter.=:=[Short] {

  override def read(reader: Reader): Short =
    reader.readShort()

  override def write(value: Short, writer: Writer): Unit =
    writer.writeShort(value)

}
