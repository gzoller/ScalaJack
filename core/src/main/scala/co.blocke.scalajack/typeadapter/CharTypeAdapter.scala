package co.blocke.scalajack
package typeadapter

object CharTypeAdapter extends SimpleTypeAdapter[Char] {

  override val isStringKind: Boolean = true

  override def read(reader: Reader): Char = {
    reader.readString().head // TODO Ensure there is only one character
  }

  override def write(value: Char, writer: Writer): Unit =
    writer.writeChar(value)

}
