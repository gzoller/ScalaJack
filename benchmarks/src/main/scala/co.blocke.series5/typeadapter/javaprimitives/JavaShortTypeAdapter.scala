package co.blocke.series5
package typeadapter
package javaprimitives

object JavaShortTypeAdapter extends SimpleTypeAdapter[java.lang.Short] {

  override def read(reader: Reader): java.lang.Short =
    reader.peek match {
      case TokenType.Number =>
        java.lang.Short.valueOf(reader.readShort())

      case TokenType.Null =>
        reader.readNull()

      case actual => {
        reader.read()
        throw new IllegalStateException(s"Expected value token of type Number, not $actual when reading Short value.  (Is your value wrapped in quotes?)\n" + reader.showError())
      }
    }

  override def write(value: java.lang.Short, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeShort(value.shortValue)
    }

}
