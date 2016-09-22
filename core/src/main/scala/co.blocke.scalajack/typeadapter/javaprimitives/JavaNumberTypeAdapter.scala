package co.blocke.scalajack
package typeadapter
package javaprimitives

object JavaNumberTypeAdapter extends SimpleTypeAdapter.ForTypeSymbolOf[java.lang.Number] {

  override def read(reader: Reader): java.lang.Number =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.Number ⇒
        reader.readNumber()
    }

  override def write(nullableValue: java.lang.Number, writer: Writer): Unit =
    nullableValue match {
      case null ⇒
        writer.writeNull()

      case value: java.lang.Integer ⇒
        writer.writeInt(value.intValue)

      case value: java.lang.Long ⇒
        writer.writeLong(value.longValue)

      case value: java.lang.Double ⇒
        writer.writeDouble(value.doubleValue)

      case value: java.lang.Float ⇒
        writer.writeFloat(value.floatValue)

      case value: java.lang.Byte ⇒
        writer.writeByte(value.byteValue)
    }

}
