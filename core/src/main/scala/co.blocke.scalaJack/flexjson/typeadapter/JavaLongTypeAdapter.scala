package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{Reader, TokenType, Writer}

object JavaLongTypeAdapter extends SimpleTypeAdapter[java.lang.Long] {

  override def read(reader: Reader): java.lang.Long =
    reader.peek match {
      case TokenType.Null ⇒
        null

      case TokenType.Number ⇒
        reader.read(expected = TokenType.Number)
        java.lang.Long.valueOf(reader.tokenText)
    }

  override def write(value: java.lang.Long, writer: Writer): Unit = ???

}
