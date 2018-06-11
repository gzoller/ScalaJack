package co.blocke.scalajack
package typeadapter
package javaprimitives

import co.blocke.scalajack.typeadapter.javaprimitives.JavaBigDecimalDeserializer.JavaBigDecimalType
import co.blocke.scalajack.typeadapter.javaprimitives.JavaBigDecimalSerializer.ScalaBigDecimalType

import scala.reflect.runtime.universe.{ Type, typeOf }

object JavaBigDecimalDeserializer {

  val JavaBigDecimalType: Type = typeOf[java.math.BigDecimal]

}

class JavaBigDecimalDeserializer(scalaBigDecimalDeserializer: Deserializer[scala.math.BigDecimal]) extends Deserializer[java.math.BigDecimal] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[java.math.BigDecimal] =
    scalaBigDecimalDeserializer.deserialize(path, json) map {
      case TypeTagged(null)            => TypeTagged(null, JavaBigDecimalType)
      case TypeTagged(scalaBigDecimal) => TypeTagged(scalaBigDecimal.bigDecimal, JavaBigDecimalType)
    }

}

object JavaBigDecimalSerializer {

  val ScalaBigDecimalType: Type = typeOf[scala.math.BigDecimal]

}

class JavaBigDecimalSerializer(scalaBigDecimalSerializer: Serializer[scala.math.BigDecimal]) extends Serializer[java.math.BigDecimal] {

  override def serialize[J](tagged: TypeTagged[java.math.BigDecimal])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(null)           => scalaBigDecimalSerializer.serialize(TypeTagged(null, ScalaBigDecimalType))
      case TypeTagged(javaBigDecimal) => scalaBigDecimalSerializer.serialize(TypeTagged(scala.math.BigDecimal(javaBigDecimal), ScalaBigDecimalType))
    }

}

object JavaBigDecimalTypeAdapter extends TypeAdapterFactory.=:=[java.math.BigDecimal] {

  override def create(next: TypeAdapterFactory)(implicit context: Context): TypeAdapter[java.math.BigDecimal] = {
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