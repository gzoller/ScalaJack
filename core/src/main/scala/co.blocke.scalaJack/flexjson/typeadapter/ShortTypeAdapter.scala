package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{ Reader, TokenType, Writer }

object ShortTypeAdapter extends SimpleTypeAdapter[Short] {

  override def read(reader: Reader): Short = {
    reader.read(expected = TokenType.Number)
    reader.tokenText.toShort
  }

  override def write(value: Short, writer: Writer): Unit =
    writer.writeShort(value)

}
