package co.blocke.scalajack.json
package typeadapter
package javaprimitives

object JavaDoubleTypeAdapter extends SimpleTypeAdapter[java.lang.Double] {

  override def read(reader: Reader): java.lang.Double =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.Number ⇒
        java.lang.Double.valueOf(reader.readDouble())
    }

  override def write(value: java.lang.Double, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeDouble(value.doubleValue)
    }

}
