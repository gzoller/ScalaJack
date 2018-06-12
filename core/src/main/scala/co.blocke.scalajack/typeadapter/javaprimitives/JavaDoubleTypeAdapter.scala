package co.blocke.scalajack
package typeadapter
package javaprimitives

import scala.reflect.runtime.universe.TypeTag

object JavaDoubleTypeAdapter extends TypeAdapterFactory.=:=[java.lang.Double] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[java.lang.Double]): TypeAdapter[java.lang.Double] = {
    val doubleTypeAdapter = context.typeAdapterOf[Double]
    new JavaDoubleTypeAdapter(
      deserializer = new BoxedDoubleDeserializer(doubleTypeAdapter.deserializer),
      serializer   = new BoxedDoubleSerializer(doubleTypeAdapter.serializer))
  }

}

class JavaDoubleTypeAdapter(override val deserializer: Deserializer[java.lang.Double], override val serializer: Serializer[java.lang.Double]) extends TypeAdapter.=:=[java.lang.Double] {

  override def read(reader: Reader): java.lang.Double =
    reader.peek match {
      case TokenType.Number =>
        java.lang.Double.valueOf(reader.readDouble())

      case TokenType.Null =>
        reader.readNull()

      case actual =>
        reader.read()
        throw new IllegalStateException(s"Expected value token of type Number, not $actual when reading Double value.  (Is your value wrapped in quotes?)\n" + reader.showError())
    }

  override def write(value: java.lang.Double, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeDouble(value.doubleValue)
    }

}
