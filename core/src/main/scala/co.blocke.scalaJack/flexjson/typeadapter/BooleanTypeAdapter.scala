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

      case TokenType.Identifier ⇒
        reader.read(expected = TokenType.Identifier)

        val source = reader.source
        val offset = reader.tokenOffset
        val length = reader.tokenLength

        if (length == 4
          && source(offset + 0) == 't'
          && source(offset + 1) == 'r'
          && source(offset + 2) == 'u'
          && source(offset + 3) == 'e') {
          true
        } else if (length == 5
          && source(offset + 0) == 'f'
          && source(offset + 1) == 'a'
          && source(offset + 2) == 'l'
          && source(offset + 3) == 's'
          && source(offset + 4) == 'e') {
          false
        } else {
          throw new IllegalArgumentException(s"Not a boolean: ${reader.tokenText}")
        }
    }
  }

  override def write(value: Boolean, writer: Writer): Unit =
    if (value) {
      writer.writeTrue()
    } else {
      writer.writeFalse()
    }

}
