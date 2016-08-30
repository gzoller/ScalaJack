package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{Reader, TokenType, Writer}

object CharTypeAdapter extends SimpleTypeAdapter[Char] {

  override def read(reader: Reader): Char = {
    reader.readString().head // TODO Ensure there is only one character
  }

  override def write(value: Char, writer: Writer): Unit =
    writer.writeChar(value)

}
