package co.blocke.scalajack
package typeadapter
package javaprimitives

import java.math.BigInteger

object JavaBigIntegerTypeAdapter extends SimpleTypeAdapter[BigInteger] {

  override def read(reader: Reader): BigInteger =
    reader.peek match {
      case TokenType.Number =>
        reader.read(expected = TokenType.Number)
        new BigInteger(reader.tokenText)

      case TokenType.Null =>
        reader.readNull()

      case actual => {
        reader.read()
        throw new IllegalStateException(s"Expected value token of type Number, not $actual when reading BigInteger value.  (Is your value wrapped in quotes?)\n" + reader.showError())
      }
    }

  override def write(value: BigInteger, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeRawValue(value.toString)
    }

}