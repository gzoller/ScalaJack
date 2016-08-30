package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{ Reader, TokenType, Writer }

object IntTypeAdapter extends SimpleTypeAdapter[Int] {

  override def read(reader: Reader): Int = {
    reader.read(expected = TokenType.Number)
    reader.tokenText.toInt
  }

  override def write(value: Int, writer: Writer): Unit =
    writer.writeInt(value)

}
