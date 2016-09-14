package co.blocke.scalajack.json
package typeadapter
package javaprimitives

object JavaFloatTypeAdapter extends SimpleTypeAdapter[java.lang.Float] {

  override def read(reader: Reader): java.lang.Float =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.Number ⇒
        java.lang.Float.valueOf(reader.readFloat())
    }

  override def write(value: java.lang.Float, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeFloat(value.floatValue)
    }

}
