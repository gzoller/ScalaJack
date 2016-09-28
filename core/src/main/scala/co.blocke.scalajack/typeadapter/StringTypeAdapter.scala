package co.blocke.scalajack
package typeadapter

object StringTypeAdapter extends SimpleTypeAdapter[String] {

  override val isStringKind: Boolean = true

  override def read(reader: Reader): String = {
    reader.peek match {
      case TokenType.String ⇒
        reader.readString()

      case TokenType.Null ⇒
        reader.readNull()

      case actual ⇒ {
        reader.read()
        throw new IllegalStateException(s"Expected value token of type String, not $actual when reading String value.\n" + reader.showError())
      }
    }
  }

  override def write(value: String, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value)
    }

}
