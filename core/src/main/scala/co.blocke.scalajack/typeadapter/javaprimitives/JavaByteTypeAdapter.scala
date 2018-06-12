package co.blocke.scalajack
package typeadapter
package javaprimitives

import scala.reflect.runtime.universe.TypeTag

object JavaByteTypeAdapter extends TypeAdapterFactory.=:=[java.lang.Byte] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[java.lang.Byte]): TypeAdapter[java.lang.Byte] = {
    val byteTypeAdapter = context.typeAdapterOf[Byte]
    new JavaByteTypeAdapter(
      deserializer = new BoxedByteDeserializer(byteTypeAdapter.deserializer),
      serializer   = new BoxedByteSerializer(byteTypeAdapter.serializer))
  }

}

class JavaByteTypeAdapter(override val deserializer: Deserializer[java.lang.Byte], override val serializer: Serializer[java.lang.Byte]) extends TypeAdapter.=:=[java.lang.Byte] {

  override def read(reader: Reader): java.lang.Byte =
    reader.peek match {
      case TokenType.Null =>
        reader.readNull()

      case TokenType.Number =>
        java.lang.Byte.valueOf(reader.readByte())

      case actual =>
        reader.read()
        throw new IllegalStateException(s"Expected value token of type Number, not $actual when reading Byte value.  (Is your value wrapped in quotes?)\n" + reader.showError())
    }

  override def write(value: java.lang.Byte, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeByte(value.byteValue)
    }

}
