package co.blocke.scalajack
package typeadapter

object BooleanTypeAdapter extends TypeAdapter.=:=[Boolean] {

  override val deserializer: Deserializer[Boolean] = new BooleanDeserializer

  override val serializer: Serializer[Boolean] = new BooleanSerializer

  override def read(reader: Reader): Boolean =
    reader.peek match {
      case TokenType.False =>
        reader.read(expected = TokenType.False)
        false

      case TokenType.True =>
        reader.read(expected = TokenType.True)
        true

      case TokenType.Null =>
        throw new IllegalStateException("Expected token of type Boolean, not Null\n" + reader.showError())

      case actual =>
        reader.read()
        throw new IllegalStateException(s"Expected value token of type True or False, not $actual when reading Boolean value.  (Is your value wrapped in quotes or a number?)\n" + reader.showError())
    }

  override def write(value: Boolean, writer: Writer): Unit =
    if (value) {
      writer.writeTrue()
    } else {
      writer.writeFalse()
    }

}
