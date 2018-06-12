package co.blocke.scalajack
package typeadapter
package javaprimitives

object JavaCharacterTypeAdapter extends TypeAdapterFactory.=:=[java.lang.Character] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[java.lang.Character]): TypeAdapter[java.lang.Character] = {
    val charTypeAdapter = context.typeAdapterOf[Char]
    new JavaCharacterTypeAdapter(
      deserializer = new BoxedCharDeserializer(charTypeAdapter.deserializer),
      serializer   = new BoxedCharSerializer(charTypeAdapter.serializer))
  }

}

class JavaCharacterTypeAdapter(override val deserializer: Deserializer[java.lang.Character], override val serializer: Serializer[java.lang.Character]) extends TypeAdapter.=:=[java.lang.Character] with StringKind {

  override def read(reader: Reader): java.lang.Character =
    reader.peek match {
      case TokenType.String =>
        java.lang.Character.valueOf(reader.readString().head)

      case TokenType.Null =>
        reader.readNull()

      case actual =>
        reader.read()
        throw new IllegalStateException(s"Expected value token of type String, not $actual when reading Character value.  (Is your value wrapped in quotes?)\n" + reader.showError())
    }

  override def write(value: java.lang.Character, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeChar(value.charValue)
    }

}
