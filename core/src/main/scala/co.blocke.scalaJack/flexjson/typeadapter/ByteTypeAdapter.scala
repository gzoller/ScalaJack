package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{ Reader, TokenType, Writer }

object ByteTypeAdapter extends SimpleTypeAdapter[Byte] {

  override def read(reader: Reader): Byte = {
    reader.read(expected = TokenType.Number)
    reader.tokenText.toByte
  }

  override def write(value: Byte, writer: Writer): Unit =
    writer.writeByte(value)

}
