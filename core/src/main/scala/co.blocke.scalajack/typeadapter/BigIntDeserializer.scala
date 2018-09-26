package co.blocke.scalajack
package typeadapter

class BigIntDeserializer extends Deserializer[BigInt] {

  self =>

  private val BigIntType: Type = typeOf[BigInt]

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J], guidance: DeserializationGuidance): DeserializationResult[BigInt] =
    json match {
      case JsonNull()           => DeserializationSuccess(TypeTagged(null, BigIntType))
      case JsonLong(longValue)  => DeserializationSuccess(TypeTagged(BigInt(longValue), BigIntType))
      case JsonInt(scalaBigInt) => DeserializationSuccess(TypeTagged(scalaBigInt, BigIntType))

      case JsonDecimal(scalaBigDecimal) =>
        DeserializationResult(path)(TypeTagged(BigInt(scalaBigDecimal.bigDecimal.toBigIntegerExact), BigIntType), {
          case e: ArithmeticException =>
            DeserializationError.Malformed(e, reportedBy = self)
        })

      case JsonDouble(doubleValue) =>
        DeserializationResult(path)(TypeTagged(BigInt(new java.math.BigDecimal(doubleValue).toBigIntegerExact), BigIntType), {
          case e: ArithmeticException =>
            DeserializationError.Malformed(e, reportedBy = self)
        })

      case _ => DeserializationFailure(path, DeserializationError.Unexpected("Expected a JSON number", reportedBy = self))
    }

}
