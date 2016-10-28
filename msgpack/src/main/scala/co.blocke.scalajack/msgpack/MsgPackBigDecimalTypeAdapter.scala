package co.blocke.scalajack
package msgpack

import typeadapter.SimpleTypeAdapter

// NOTE: This class is needed because msgpack does not support BigDecimal types so we have to consiously treat them
// as a string of number-characters or otherwise face losing fidelity.

object MsgPackBigDecimalTypeAdapter extends SimpleTypeAdapter[BigDecimal] {

  override def read(reader: Reader): BigDecimal =
    reader.peek match {
      case TokenType.String ⇒
        // reader.read(expected = TokenType.Number)
        BigDecimal(reader.readString)

      case TokenType.Null ⇒
        reader.readNull()

      case actual ⇒ {
        reader.read()
        throw new IllegalStateException(s"Expected value token of type Number, not $actual when reading BigDecimal value.  (Is your value wrapped in quotes?)\n" + reader.showError())
      }
    }

  override def write(value: BigDecimal, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value.toString)
    }

}
