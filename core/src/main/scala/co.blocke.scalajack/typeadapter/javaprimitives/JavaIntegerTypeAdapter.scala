package co.blocke.scalajack
package typeadapter
package javaprimitives

object JavaIntegerTypeAdapter extends SimpleTypeAdapter.ForTypeSymbolOf[java.lang.Integer] {

  override def read(reader: Reader): java.lang.Integer =
    reader.peek match {
      case TokenType.Number =>
        java.lang.Integer.valueOf(reader.readInt())

      case TokenType.Null =>
        reader.readNull()

      case actual => {
        reader.read()
        throw new IllegalStateException(s"Expected value token of type Number, not $actual when reading Integer value.  (Is your value wrapped in quotes?)\n" + reader.showError())
      }
    }

  override def write(value: java.lang.Integer, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeInt(value.intValue)
    }

}
