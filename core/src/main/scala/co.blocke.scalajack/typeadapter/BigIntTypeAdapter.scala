package co.blocke.scalajack
package typeadapter

object BigIntTypeAdapter extends SimpleTypeAdapter[BigInt] {

  override def read(reader: Reader): BigInt =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.Number ⇒
        reader.read(expected = TokenType.Number)
        BigInt(reader.tokenText)

      case actual ⇒ {
        reader.read()
        throw new IllegalStateException(s"Expected value token of type Number, not $actual when reading BigInt value.  (Is your value wrapped in quotes?)\n" + reader.showError())
      }
    }

  override def write(value: BigInt, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeRawValue(value.toString)
    }

}
