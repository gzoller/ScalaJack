package co.blocke.scalajack
package typeadapter
package javaprimitives

object JavaShortTypeAdapter extends TypeAdapterFactory.=:=[java.lang.Short] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[java.lang.Short]): TypeAdapter[java.lang.Short] = {
    val shortTypeAdapter = context.typeAdapterOf[Short]
    new JavaShortTypeAdapter(
      deserializer = new BoxedShortDeserializer(shortTypeAdapter.deserializer),
      serializer   = new BoxedShortSerializer(shortTypeAdapter.serializer))
  }

}

class JavaShortTypeAdapter(override val deserializer: Deserializer[java.lang.Short], override val serializer: Serializer[java.lang.Short]) extends TypeAdapter[java.lang.Short] {

  override def read(reader: Reader): java.lang.Short =
    reader.peek match {
      case TokenType.Number =>
        java.lang.Short.valueOf(reader.readShort())

      case TokenType.Null =>
        reader.readNull()

      case actual =>
        reader.read()
        throw new IllegalStateException(s"Expected value token of type Number, not $actual when reading Short value.  (Is your value wrapped in quotes?)\n" + reader.showError())
    }

  override def write(value: java.lang.Short, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeShort(value.shortValue)
    }

}
