package co.blocke.scalajack
package msgpack
package typeadapter

import co.blocke.scalajack.typeadapter.SimpleTypeAdapter

// NOTE: This class is needed because msgpack does not support BigInt types so we have to consiously treat them
// as a string of number-characters or otherwise face losing fidelity.

object MsgPackBigIntTypeAdapter extends SimpleTypeAdapter[BigInt] {

  override def read(reader: Reader): BigInt =
    reader.peek match {
      case TokenType.String ⇒
        // reader.read(expected = TokenType.Number)
        BigInt(reader.readString)

      case TokenType.Null ⇒
        reader.readNull()

      case actual ⇒ {
        reader.read()
        throw new IllegalStateException(s"Expected value token of type Number, not $actual when reading BigInt value.  (Is your value wrapped in quotes?)\n" + reader.showError())
      }
    }

  override def write(value: BigInt, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value.toString)
    }

}
