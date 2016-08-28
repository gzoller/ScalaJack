package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{Reader, TokenType, Writer}

object JavaByteTypeAdapter extends SimpleTypeAdapter[java.lang.Byte] {

  override def read(reader: Reader): java.lang.Byte =
    reader.peek match {
      case TokenType.Null ⇒
        null

      case TokenType.Number ⇒
        reader.read(expected = TokenType.Number)
        java.lang.Byte.valueOf(reader.tokenText)
    }

  override def write(value: java.lang.Byte, writer: Writer): Unit = ???

}
