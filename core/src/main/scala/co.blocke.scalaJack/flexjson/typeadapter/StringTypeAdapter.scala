package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.Unicode
import co.blocke.scalajack.flexjson.{ Reader, TokenType, Writer }

object StringTypeAdapter extends SimpleTypeAdapter[String] {

  override def read(reader: Reader): String = {
    reader.peek match {
      case TokenType.String ⇒
        Unicode.unescape_perl_string(reader.readString())

      case TokenType.Identifier ⇒
        reader.readIdentifier()

      case TokenType.Null ⇒
        reader.readNull()
    }
  }

  override def write(value: String, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value)
    }

}
