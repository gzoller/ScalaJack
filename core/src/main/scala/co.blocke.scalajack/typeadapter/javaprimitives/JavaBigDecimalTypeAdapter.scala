package co.blocke.scalajack
package typeadapter
package javaprimitives

import scala.reflect.runtime.universe.TypeTag

object JavaBigDecimalTypeAdapter extends TypeAdapterFactory.=:=[java.math.BigDecimal] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[java.math.BigDecimal]): TypeAdapter[java.math.BigDecimal] = {
    val scalaBigDecimalTypeAdapter = context.typeAdapterOf[scala.math.BigDecimal]
    new JavaBigDecimalTypeAdapter(
      deserializer = new JavaBigDecimalDeserializer(scalaBigDecimalTypeAdapter.deserializer),
      serializer   = new JavaBigDecimalSerializer(scalaBigDecimalTypeAdapter.serializer))
  }

}

class JavaBigDecimalTypeAdapter(override val deserializer: Deserializer[java.math.BigDecimal], override val serializer: Serializer[java.math.BigDecimal]) extends TypeAdapter.=:=[java.math.BigDecimal] {

  override def read(reader: Reader): java.math.BigDecimal =
    reader.peek match {
      case TokenType.Number =>
        reader.read(expected = TokenType.Number)
        new java.math.BigDecimal(reader.tokenText)

      case TokenType.Null =>
        reader.readNull()

      case actual =>
        reader.read()
        throw new IllegalStateException(s"Expected value token of type Number, not $actual when reading BigDecimal value.  (Is your value wrapped in quotes?)\n" + reader.showError())
    }

  override def write(value: java.math.BigDecimal, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeRawValue(value.toString)
    }

}