package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{ Reader, TokenType, Writer }

object LongTypeAdapter extends SimpleTypeAdapter[Long] {

  override def read(reader: Reader): Long = {
    reader.read(expected = TokenType.Number)
    reader.tokenText.toLong
  }

  override def write(value: Long, writer: Writer): Unit =
    writer.writeLong(value)

}
