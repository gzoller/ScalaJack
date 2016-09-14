package co.blocke.scalajack.json
package typeadapter

object BigDecimalTypeAdapter extends SimpleTypeAdapter[BigDecimal] {

  override def read(reader: Reader): BigDecimal =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.Number ⇒
        reader.read(expected = TokenType.Number)
        BigDecimal(reader.tokenText)
    }

  override def write(value: BigDecimal, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeRawValue(value.toString)
    }

}
