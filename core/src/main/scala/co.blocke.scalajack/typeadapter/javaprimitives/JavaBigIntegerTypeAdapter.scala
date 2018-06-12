package co.blocke.scalajack
package typeadapter
package javaprimitives

object JavaBigIntegerTypeAdapter extends TypeAdapterFactory.=:=[java.math.BigInteger] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[java.math.BigInteger]): TypeAdapter[java.math.BigInteger] = {
    val scalaBigIntTypeAdapter = context.typeAdapterOf[scala.math.BigInt]
    new JavaBigIntegerTypeAdapter(
      deserializer = new JavaBigIntegerDeserializer(scalaBigIntTypeAdapter.deserializer),
      serializer   = new JavaBigIntegerSerializer(scalaBigIntTypeAdapter.serializer))
  }

}

class JavaBigIntegerTypeAdapter(override val deserializer: Deserializer[java.math.BigInteger], override val serializer: Serializer[java.math.BigInteger]) extends TypeAdapter[java.math.BigInteger] {

  override def read(reader: Reader): java.math.BigInteger =
    reader.peek match {
      case TokenType.Number =>
        reader.read(expected = TokenType.Number)
        new java.math.BigInteger(reader.tokenText)

      case TokenType.Null =>
        reader.readNull()

      case actual =>
        reader.read()
        throw new IllegalStateException(s"Expected value token of type Number, not $actual when reading BigInteger value.  (Is your value wrapped in quotes?)\n" + reader.showError())
    }

  override def write(value: java.math.BigInteger, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeRawValue(value.toString)
    }

}
