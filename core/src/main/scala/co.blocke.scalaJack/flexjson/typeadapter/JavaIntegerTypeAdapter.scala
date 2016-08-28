package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{Reader, TokenType, Writer}

object JavaIntegerTypeAdapter extends SimpleTypeAdapter[java.lang.Integer] {

  override def read(reader: Reader): java.lang.Integer =
    reader.peek match {
      case TokenType.Null ⇒
        null

      case TokenType.Number ⇒
        reader.read(expected = TokenType.Number)
        java.lang.Integer.valueOf(reader.tokenText)
    }

  override def write(value: java.lang.Integer, writer: Writer): Unit = ???

}
