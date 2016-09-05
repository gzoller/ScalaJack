package co.blocke.scalajack.flexjson.typeadapter.javaprimitives

import co.blocke.scalajack.flexjson.typeadapter.SimpleTypeAdapter
import co.blocke.scalajack.flexjson.{ Reader, TokenType, Writer }

object JavaBooleanTypeAdapter extends SimpleTypeAdapter[java.lang.Boolean] {

  override def read(reader: Reader): java.lang.Boolean =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case _ ⇒
        java.lang.Boolean.valueOf(reader.readBoolean())
    }

  override def write(value: java.lang.Boolean, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeBoolean(value.booleanValue)
    }

}
