package co.blocke.scalajack.flexjson.typeadapter.java

import co.blocke.scalajack.flexjson.typeadapter.SimpleTypeAdapter
import co.blocke.scalajack.flexjson.{Reader, TokenType, Writer}

object JavaFloatTypeAdapter extends SimpleTypeAdapter[java.lang.Float] {

  override def read(reader: Reader): java.lang.Float =
    reader.peek match {
      case TokenType.Null ⇒
        null

      case TokenType.Number ⇒
        reader.read(expected = TokenType.Number)
        java.lang.Float.valueOf(reader.tokenText)
    }

  override def write(value: java.lang.Float, writer: Writer): Unit = ???

}
