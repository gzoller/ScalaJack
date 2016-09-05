package co.blocke.scalajack.flexjson.typeadapter.javaprimitives

import co.blocke.scalajack.flexjson.typeadapter.SimpleTypeAdapter
import co.blocke.scalajack.flexjson.{ Reader, TokenType, Writer }

object JavaIntegerTypeAdapter extends SimpleTypeAdapter[java.lang.Integer] {

  override def read(reader: Reader): java.lang.Integer =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.Number ⇒
        java.lang.Integer.valueOf(reader.readInt())
    }

  override def write(value: java.lang.Integer, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeInt(value.intValue)
    }

}
