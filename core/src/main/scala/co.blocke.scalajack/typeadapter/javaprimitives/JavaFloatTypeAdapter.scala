package co.blocke.scalajack
package typeadapter
package javaprimitives

object JavaFloatTypeAdapter extends SimpleTypeAdapter[java.lang.Float] {

  override def read(reader: Reader): java.lang.Float =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.Number ⇒
        java.lang.Float.valueOf(reader.readFloat())

      case actual ⇒ {
        reader.read()
        throw new IllegalStateException(s"Expected value token of type Number, not $actual when reading Float value.  (Is your value wrapped in quotes?)\n" + reader.showError())
      }
    }

  override def write(value: java.lang.Float, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeFloat(value.floatValue)
    }

}
