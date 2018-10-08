package co.blocke.scalajack
package typeadapter

/* This is basically like the java primitive equivalent, except this marshals Scala primitives */
case class NumberDeserializer() extends Deserializer[java.lang.Number] {

  self =>

  private val BoxedNumberType: Type = typeOf[java.lang.Number]
  private val BoxedDoubleType: Type = typeOf[Double]
  private val BoxedLongType: Type = typeOf[Long]
  private val ScalaBigDecimalType: Type = typeOf[scala.math.BigDecimal]
  private val ScalaBigIntegerType: Type = typeOf[scala.math.BigInt]

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J], guidance: SerializationGuidance): DeserializationResult[java.lang.Number] =
    json match {
      case JsonNull() => DeserializationSuccess(TypeTagged(null, BoxedNumberType))
      case JsonDecimal(scalaBigDecimal) if (!scalaBigDecimal.isDecimalDouble) => DeserializationSuccess(TypeTagged(scalaBigDecimal, ScalaBigDecimalType))
      case JsonDecimal(scalaBigDecimal) => DeserializationSuccess(TypeTagged(scalaBigDecimal.doubleValue(), BoxedDoubleType))
      case JsonDouble(doubleValue) => DeserializationSuccess(TypeTagged(java.lang.Double.valueOf(doubleValue), BoxedDoubleType))
      case JsonInt(scalaBigInt) => DeserializationSuccess(TypeTagged(scalaBigInt, ScalaBigIntegerType))
      case JsonLong(longValue) => DeserializationSuccess(TypeTagged(java.lang.Long.valueOf(longValue), BoxedLongType))
      case JsonString(s) if (guidance.isMapKey) => this.deserialize(path, ops.parse(s))(ops, guidance = guidance.copy(isMapKey = false))
      case _ => DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON number", reportedBy = self))
    }
}
