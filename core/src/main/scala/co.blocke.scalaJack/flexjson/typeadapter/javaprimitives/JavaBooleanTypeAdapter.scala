package co.blocke.scalajack.flexjson.typeadapter.javaprimitives

import co.blocke.scalajack.flexjson.typeadapter.SimpleTypeAdapter
import co.blocke.scalajack.flexjson.{Reader, TokenType, Writer}

object JavaBooleanTypeAdapter extends SimpleTypeAdapter[java.lang.Boolean] {

  override def read(reader: Reader): java.lang.Boolean =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.True ⇒
        reader.read(expected = TokenType.True)
        java.lang.Boolean.TRUE

      case TokenType.False ⇒
        reader.read(expected = TokenType.False)
        java.lang.Boolean.FALSE
    }

  override def write(value: java.lang.Boolean, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeBoolean(value.booleanValue)
    }

}
