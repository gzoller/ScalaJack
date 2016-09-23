package co.blocke.scalajack
package typeadapter
package javaprimitives

object JavaNumberTypeAdapter extends SimpleTypeAdapter.ForTypeSymbolOf[java.lang.Number] {

  override def read(reader: Reader): java.lang.Number =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.Number ⇒
        reader.readNumber(true)

      case actual ⇒ {
        reader.read()
        throw new IllegalStateException(s"Expected value token of type Number, not $actual when reading Number value.  (Is your value wrapped in quotes?)\n" + reader.showError())
      }
    }

  override def write(nullableValue: java.lang.Number, writer: Writer): Unit =
    nullableValue match {
      case null ⇒
        writer.writeNull()

      case value: java.lang.Byte ⇒
        writer.writeByte(value.byteValue)

      case value: java.lang.Double ⇒
        writer.writeDouble(value.doubleValue)

      case value: java.lang.Float ⇒
        writer.writeFloat(value.floatValue)

      case value: java.lang.Integer ⇒
        writer.writeInt(value.intValue)

      case value: java.lang.Long ⇒
        writer.writeLong(value.longValue)

      case value: java.lang.Short ⇒
        writer.writeShort(value.shortValue)

      case value: java.math.BigInteger ⇒
        writer.writeRawValue(value.toString)

      case value: java.math.BigDecimal ⇒
        writer.writeRawValue(value.toString)

    }

}
