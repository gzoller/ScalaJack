package co.blocke.scalajack
package typeadapter
package javaprimitives

object JavaDoubleTypeAdapter extends SimpleTypeAdapter[java.lang.Double] {

  override def read(reader: Reader): java.lang.Double =
    reader.peek match {
      case TokenType.Number =>
        java.lang.Double.valueOf(reader.readDouble())

      case TokenType.Null =>
        reader.readNull()

      case actual => {
        reader.read()
        throw new IllegalStateException(s"Expected value token of type Number, not $actual when reading Double value.  (Is your value wrapped in quotes?)\n" + reader.showError())
      }
    }

  override def write(value: java.lang.Double, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeDouble(value.doubleValue)
    }

}
