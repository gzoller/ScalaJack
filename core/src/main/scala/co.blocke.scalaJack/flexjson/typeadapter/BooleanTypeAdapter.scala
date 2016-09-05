package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{ Reader, TokenType, Writer }

object BooleanTypeAdapter extends SimpleTypeAdapter[Boolean] {

  override def read(reader: Reader): Boolean = {
    reader.peek match {
      case TokenType.False ⇒
        reader.read(expected = TokenType.False)
        false

      case TokenType.True ⇒
        reader.read(expected = TokenType.True)
        true

      case TokenType.Null ⇒
        throw new IllegalStateException("Expected token of type Boolean, not Null")
    }
  }

  override def write(value: Boolean, writer: Writer): Unit =
    if (value) {
      writer.writeTrue()
    } else {
      writer.writeFalse()
    }

}
