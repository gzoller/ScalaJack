package co.blocke.scalajack.json
package typeadapter
package javaprimitives

object JavaCharacterTypeAdapter extends SimpleTypeAdapter[java.lang.Character] {

  override def read(reader: Reader): java.lang.Character =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.String ⇒
        java.lang.Character.valueOf(reader.readString().head)
    }

  override def write(value: java.lang.Character, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeChar(value.charValue)
    }

}
