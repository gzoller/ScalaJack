package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{ Reader, Writer }

object ShortTypeAdapter extends SimpleTypeAdapter[Short] {

  override def read(reader: Reader): Short =
    reader.readShort()

  override def write(value: Short, writer: Writer): Unit =
    writer.writeShort(value)

}
