package co.blocke.scalajack.json
package typeadapter
package javaprimitives

object JavaByteTypeAdapter extends SimpleTypeAdapter[java.lang.Byte] {

  override def read(reader: Reader): java.lang.Byte =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.Number ⇒
        java.lang.Byte.valueOf(reader.readByte())
    }

  override def write(value: java.lang.Byte, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeByte(value.byteValue)
    }

}
