package co.blocke.scalajack
package typeadapter
package javaprimitives

import co.blocke.scalajack.typeadapter.javaprimitives.BoxedNumberDeserializer.{ BoxedDoubleType, BoxedLongType, BoxedNumberType, JavaBigDecimalType, JavaBigIntegerType }

import scala.reflect.runtime.universe.{ Type, typeOf }

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
