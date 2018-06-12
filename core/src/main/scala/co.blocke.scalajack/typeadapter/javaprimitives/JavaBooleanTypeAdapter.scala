package co.blocke.scalajack
package typeadapter
package javaprimitives

import scala.reflect.runtime.universe.TypeTag

object JavaBooleanTypeAdapter extends TypeAdapterFactory.=:=[java.lang.Boolean] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[java.lang.Boolean]): TypeAdapter[java.lang.Boolean] = {
    val booleanTypeAdapter = context.typeAdapterOf[Boolean]
    new JavaBooleanTypeAdapter(
      deserializer = new BoxedBooleanDeserializer(booleanTypeAdapter.deserializer),
      serializer   = new BoxedBooleanSerializer(booleanTypeAdapter.serializer))
  }

}

class JavaBooleanTypeAdapter(override val deserializer: Deserializer[java.lang.Boolean], override val serializer: Serializer[java.lang.Boolean]) extends TypeAdapter[java.lang.Boolean] {

  override def read(reader: Reader): java.lang.Boolean =
    reader.peek match {
      case TokenType.False | TokenType.True =>
        java.lang.Boolean.valueOf(reader.readBoolean())

      case TokenType.Null =>
        reader.readNull()

      case actual =>
        reader.read()
        throw new IllegalStateException(s"Expected value token of type True or False, not $actual when reading Boolean value.  (Is your value wrapped in quotes?)\n" + reader.showError())
    }

  override def write(value: java.lang.Boolean, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeBoolean(value.booleanValue)
    }

}
