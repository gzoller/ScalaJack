package co.blocke.series5
package typeadapter
package javaprimitives

object JavaByteTypeAdapter extends SimpleTypeAdapter[java.lang.Byte] {

  override def read(reader: Reader): java.lang.Byte =
    reader.peek match {
      case TokenType.Null =>
        reader.readNull()

      case TokenType.Number =>
        java.lang.Byte.valueOf(reader.readByte())

      case actual => {
        reader.read()
        throw new IllegalStateException(s"Expected value token of type Number, not $actual when reading Byte value.  (Is your value wrapped in quotes?)\n" + reader.showError())
      }
    }

  override def write(value: java.lang.Byte, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeByte(value.byteValue)
    }

}
