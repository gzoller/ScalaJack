package co.blocke.scalajack
package typeadapter
package javaprimitives

import scala.reflect.runtime.universe.TypeTag

object JavaFloatTypeAdapter extends TypeAdapterFactory.=:=[java.lang.Float] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[java.lang.Float]): TypeAdapter[java.lang.Float] = {
    val floatTypeAdapter = context.typeAdapterOf[Float]
    new JavaFloatTypeAdapter(
      deserializer = new BoxedFloatDeserializer(floatTypeAdapter.deserializer),
      serializer   = new BoxedFloatSerializer(floatTypeAdapter.serializer))
  }

}

class JavaFloatTypeAdapter(override val deserializer: Deserializer[java.lang.Float], override val serializer: Serializer[java.lang.Float]) extends TypeAdapter.=:=[java.lang.Float] {

  override def read(reader: Reader): java.lang.Float =
    reader.peek match {
      case TokenType.Number =>
        java.lang.Float.valueOf(reader.readFloat())

      case TokenType.Null =>
        reader.readNull()

      case actual =>
        reader.read()
        throw new IllegalStateException(s"Expected value token of type Number, not $actual when reading Float value.  (Is your value wrapped in quotes?)\n" + reader.showError())
    }

  override def write(value: java.lang.Float, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeFloat(value.floatValue)
    }

}
