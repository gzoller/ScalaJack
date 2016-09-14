package co.blocke.scalajack.json
package typeadapter
package javaprimitives

object JavaIntegerTypeAdapter extends SimpleTypeAdapter.ForTypeSymbolOf[java.lang.Integer] {

  override def read(reader: Reader): java.lang.Integer =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.Number ⇒
        java.lang.Integer.valueOf(reader.readInt())
    }

  override def write(value: java.lang.Integer, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeInt(value.intValue)
    }

}
