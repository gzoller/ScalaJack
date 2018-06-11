package co.blocke.scalajack
package typeadapter
package javaprimitives

import co.blocke.scalajack.typeadapter.javaprimitives.BoxedNumberDeserializer.{ BoxedDoubleType, BoxedLongType, BoxedNumberType, JavaBigDecimalType, JavaBigIntegerType }

import scala.reflect.runtime.universe.{ Type, TypeTag, typeOf }

object BoxedNumberDeserializer {

  val BoxedNumberType: Type = typeOf[java.lang.Number]
  val BoxedDoubleType: Type = typeOf[java.lang.Double]
  val BoxedLongType: Type = typeOf[java.lang.Long]
  val JavaBigDecimalType: Type = typeOf[java.math.BigDecimal]
  val JavaBigIntegerType: Type = typeOf[java.math.BigInteger]

}

class BoxedNumberDeserializer() extends Deserializer[java.lang.Number] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[java.lang.Number] =
    json match {
      case JsonNull()                   => DeserializationSuccess(TypeTagged(null, BoxedNumberType))
      case JsonDecimal(scalaBigDecimal) => DeserializationSuccess(TypeTagged(scalaBigDecimal.bigDecimal, JavaBigDecimalType))
      case JsonDouble(doubleValue)      => DeserializationSuccess(TypeTagged(java.lang.Double.valueOf(doubleValue), BoxedDoubleType))
      case JsonInt(scalaBigInt)         => DeserializationSuccess(TypeTagged(scalaBigInt.bigInteger, JavaBigIntegerType))
      case JsonLong(longValue)          => DeserializationSuccess(TypeTagged(java.lang.Long.valueOf(longValue), BoxedLongType))
      case _                            => DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON number"))
    }

}

class BoxedNumberSerializer() extends Serializer[java.lang.Number] {

  override def serialize[J](tagged: TypeTagged[java.lang.Number])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(null)                                 => SerializationSuccess(JsonNull())
      case TypeTagged(boxedByte: java.lang.Byte)            => SerializationSuccess(JsonLong(boxedByte.longValue))
      case TypeTagged(boxedDouble: java.lang.Double)        => SerializationSuccess(JsonDouble(boxedDouble.doubleValue))
      case TypeTagged(boxedFloat: java.lang.Float)          => SerializationSuccess(JsonDouble(boxedFloat.doubleValue))
      case TypeTagged(boxedInt: java.lang.Integer)          => SerializationSuccess(JsonLong(boxedInt.longValue))
      case TypeTagged(boxedLong: java.lang.Long)            => SerializationSuccess(JsonLong(boxedLong.longValue))
      case TypeTagged(boxedShort: java.lang.Short)          => SerializationSuccess(JsonLong(boxedShort.longValue))
      case TypeTagged(javaBigInteger: java.math.BigInteger) => SerializationSuccess(JsonInt(scala.math.BigInt(javaBigInteger)))
      case TypeTagged(javaBigDecimal: java.math.BigDecimal) => SerializationSuccess(JsonDecimal(scala.math.BigDecimal(javaBigDecimal)))
    }

}

object JavaNumberTypeAdapter extends TypeAdapterFactory.=:=[java.lang.Number] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[java.lang.Number]): TypeAdapter[Number] =
    new JavaNumberTypeAdapter(deserializer = new BoxedNumberDeserializer, serializer = new BoxedNumberSerializer)

}

class JavaNumberTypeAdapter(override val deserializer: Deserializer[java.lang.Number], override val serializer: Serializer[java.lang.Number]) extends SimpleTypeAdapter.ForTypeSymbolOf[java.lang.Number] {

  override def read(reader: Reader): java.lang.Number =
    reader.peek match {
      case TokenType.Number =>
        reader.readNumber(true)

      case TokenType.Null =>
        reader.readNull()

      case actual =>
        reader.read()
        throw new IllegalStateException(s"Expected value token of type Number, not $actual when reading Number value.  (Is your value wrapped in quotes?)\n" + reader.showError())
    }

  override def write(nullableValue: java.lang.Number, writer: Writer): Unit =
    nullableValue match {
      case null =>
        writer.writeNull()

      case value: java.lang.Byte =>
        writer.writeByte(value.byteValue)

      case value: java.lang.Double =>
        writer.writeDouble(value.doubleValue)

      case value: java.lang.Float =>
        writer.writeFloat(value.floatValue)

      case value: java.lang.Integer =>
        writer.writeInt(value.intValue)

      case value: java.lang.Long =>
        writer.writeLong(value.longValue)

      case value: java.lang.Short =>
        writer.writeShort(value.shortValue)

      case value: java.math.BigInteger =>
        writer.writeRawValue(value.toString)

      case value: java.math.BigDecimal =>
        writer.writeRawValue(value.toString)

    }

}
