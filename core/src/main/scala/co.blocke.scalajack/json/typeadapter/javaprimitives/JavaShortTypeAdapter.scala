package co.blocke.scalajack.json
package typeadapter
package javaprimitives

object JavaShortTypeAdapter extends SimpleTypeAdapter[java.lang.Short] {

  override def read(reader: Reader): java.lang.Short =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.Number ⇒
        java.lang.Short.valueOf(reader.readShort())
    }

  override def write(value: java.lang.Short, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeShort(value.shortValue)
    }

}
