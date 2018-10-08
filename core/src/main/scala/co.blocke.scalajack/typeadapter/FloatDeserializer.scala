package co.blocke.scalajack
package typeadapter

class FloatDeserializer extends Deserializer[Float] {

  self =>

  import NumberConverters._

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J], guidance: SerializationGuidance): DeserializationResult[Float] =
    json match {
      case JsonDecimal(bigDecimal) if bigDecimal.isDecimalFloat =>
        DeserializationResult(path)(TypeTagged(bigDecimal.toFloat), {
          case e: ArithmeticException =>
            DeserializationError.Malformed(e, reportedBy = self)
        })
      case JsonDecimal(_) => DeserializationFailure(path, DeserializationError.Unexpected("XFloat value out of range", reportedBy = self))

      case JsonDouble(doubleValue) =>
        DeserializationResult(path)(TypeTagged(doubleValue.toFloat), {
          case e: ArithmeticException =>
            DeserializationError.Malformed(e, reportedBy = self)
        })

      case JsonLong(longValue) if (longValue >= java.lang.Float.MIN_VALUE && longValue <= java.lang.Float.MAX_VALUE) =>
        DeserializationResult(path)(TypeTagged(longValue.toFloat), {
          case e: ArithmeticException =>
            DeserializationError.Malformed(e, reportedBy = self)
        })
      case JsonLong(_) => DeserializationFailure(path, DeserializationError.Unexpected("Float value out of range", reportedBy = self))

      case JsonInt(intValue) =>
        DeserializationResult(path)(TypeTagged(intValue.toFloat), {
          case e: ArithmeticException =>
            DeserializationError.Malformed(e, reportedBy = self)
        })

      case JsonString(s) if (guidance.isMapKey) => this.deserialize(path, ops.parse(s))(ops, guidance = guidance.copy(isMapKey = false))

      case _ =>
        DeserializationFailure(path, DeserializationError.Unexpected("Expected a JSON number", reportedBy = self))
    }

}
