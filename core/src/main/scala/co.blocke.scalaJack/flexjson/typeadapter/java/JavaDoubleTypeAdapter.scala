package co.blocke.scalajack.flexjson.typeadapter.java

import co.blocke.scalajack.flexjson.typeadapter.SimpleTypeAdapter
import co.blocke.scalajack.flexjson.{Reader, TokenType, Writer}

object JavaDoubleTypeAdapter extends SimpleTypeAdapter[java.lang.Double] {

  override def read(reader: Reader): java.lang.Double =
    reader.peek match {
      case TokenType.Null ⇒
        null

      case TokenType.Number ⇒
        reader.read(expected = TokenType.Number)
        java.lang.Double.valueOf(reader.tokenText)
    }

  override def write(value: java.lang.Double, writer: Writer): Unit = ???

}
