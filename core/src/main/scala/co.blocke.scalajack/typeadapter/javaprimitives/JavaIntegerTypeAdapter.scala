package co.blocke.scalajack
package typeadapter
package javaprimitives

import scala.reflect.runtime.universe.TypeTag

object JavaIntegerTypeAdapter extends TypeAdapterFactory.=:=[java.lang.Integer] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[java.lang.Integer]): TypeAdapter[java.lang.Integer] = {
    val intTypeAdapter = context.typeAdapterOf[Int]
    new JavaIntegerTypeAdapter(
      deserializer = new BoxedIntDeserializer(intTypeAdapter.deserializer),
      serializer   = new BoxedIntSerializer(intTypeAdapter.serializer))
  }

}

class JavaIntegerTypeAdapter(override val deserializer: Deserializer[java.lang.Integer], override val serializer: Serializer[java.lang.Integer]) extends TypeAdapter[java.lang.Integer] {

  override def read(reader: Reader): java.lang.Integer =
    reader.peek match {
      case TokenType.Number =>
        java.lang.Integer.valueOf(reader.readInt())

      case TokenType.Null =>
        reader.readNull()

      case actual =>
        reader.read()
        throw new IllegalStateException(s"Expected value token of type Number, not $actual when reading Integer value.  (Is your value wrapped in quotes?)\n" + reader.showError())
    }

  override def write(value: java.lang.Integer, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeInt(value.intValue)
    }

}
