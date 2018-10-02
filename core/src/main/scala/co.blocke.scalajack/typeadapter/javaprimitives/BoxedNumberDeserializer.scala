package co.blocke.scalajack
package typeadapter
package javaprimitives

class BoxedNumberDeserializer() extends Deserializer[java.lang.Number] {

  self =>

  private val BoxedNumberType: Type = typeOf[java.lang.Number]
  private val BoxedDoubleType: Type = typeOf[java.lang.Double]
  private val BoxedLongType: Type = typeOf[java.lang.Long]
  private val BoxedFloatType: Type = typeOf[java.lang.Float]
  private val JavaBigDecimalType: Type = typeOf[java.math.BigDecimal]
  private val JavaBigIntegerType: Type = typeOf[java.math.BigInteger]

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J], guidance: SerializationGuidance): DeserializationResult[java.lang.Number] =
    json match {
      case JsonNull() => DeserializationSuccess(TypeTagged(null, BoxedNumberType))
      case JsonDecimal(scalaBigDecimal) if (!scalaBigDecimal.isDecimalDouble) => DeserializationSuccess(TypeTagged(scalaBigDecimal.bigDecimal, JavaBigDecimalType))
      case JsonDecimal(scalaBigDecimal) => DeserializationSuccess(TypeTagged(scalaBigDecimal.doubleValue, BoxedDoubleType))
      case JsonDouble(doubleValue) => DeserializationSuccess(TypeTagged(java.lang.Double.valueOf(doubleValue), BoxedDoubleType))
      case JsonInt(scalaBigInt) => DeserializationSuccess(TypeTagged(scalaBigInt.bigInteger, JavaBigIntegerType))
      case JsonLong(longValue) => DeserializationSuccess(TypeTagged(java.lang.Long.valueOf(longValue), BoxedLongType))
      case _ => DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON number", reportedBy = self))
    }
}
