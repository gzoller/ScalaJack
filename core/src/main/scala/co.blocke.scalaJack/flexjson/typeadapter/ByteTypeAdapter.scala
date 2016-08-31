package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{ Reader, Writer }

object ByteTypeAdapter extends SimpleTypeAdapter[Byte] {

  override def read(reader: Reader): Byte =
    reader.readByte()

  override def write(value: Byte, writer: Writer): Unit =
    writer.writeByte(value)

}
