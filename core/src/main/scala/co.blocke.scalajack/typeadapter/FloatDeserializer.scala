package co.blocke.scalajack
package typeadapter

class FloatDeserializer extends Deserializer[Float] {

  self =>

  import NumberConverters._

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J], guidance: DeserializationGuidance): DeserializationResult[Float] =
    json match {
      case JsonDecimal(bigDecimal) =>
        DeserializationResult(path)(TypeTagged(bigDecimal.toFloat), {
          case e: ArithmeticException =>
            DeserializationError.Malformed(e, reportedBy = self)
        })

      case JsonDouble(doubleValue) =>
        DeserializationResult(path)(TypeTagged(doubleValue.toFloat), {
          case e: ArithmeticException =>
            DeserializationError.Malformed(e, reportedBy = self)
        })

      case JsonLong(longValue) =>
        DeserializationResult(path)(TypeTagged(longValue.toFloat), {
          case e: ArithmeticException =>
            DeserializationError.Malformed(e, reportedBy = self)
        })

      case _ =>
        DeserializationFailure(path, DeserializationError.Unexpected("Expected a JSON number", reportedBy = self))
    }

}
