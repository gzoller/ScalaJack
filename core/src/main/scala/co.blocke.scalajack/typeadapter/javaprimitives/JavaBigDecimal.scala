package co.blocke.scalajack
package typeadapter
package javaprimitives

import java.math.BigDecimal

object JavaBigDecimalTypeAdapter extends SimpleTypeAdapter[BigDecimal] {

  override def read(reader: Reader): BigDecimal =
    reader.peek match {
      case TokenType.Number ⇒
        reader.read(expected = TokenType.Number)
        new BigDecimal(reader.tokenText)

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
      writer.writeRawValue(value.toString)
    }

}