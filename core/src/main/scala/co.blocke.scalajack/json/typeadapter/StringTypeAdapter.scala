package co.blocke.scalajack.json
package typeadapter

object StringTypeAdapter extends SimpleTypeAdapter[String] {

  override def read(reader: Reader): String = {
    reader.peek match {
      case TokenType.String ⇒
        reader.readString()

      case TokenType.Null ⇒
        reader.readNull()
    }
  }

  override def write(value: String, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value)
    }

}
