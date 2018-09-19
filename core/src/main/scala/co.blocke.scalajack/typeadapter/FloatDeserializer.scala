package co.blocke.scalajack
package typeadapter

class FloatDeserializer extends Deserializer[Float] {

  self =>

  import NumberConverters._

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[Float] =
    json match {
      case JsonDecimal(bigDecimal) =>
        DeserializationResult(path)(TypeTagged(bigDecimal.toFloatExact), {
          case e: ArithmeticException =>
            DeserializationError.Malformed(e, reportedBy = self)
        })

      case JsonDouble(doubleValue) =>
        DeserializationResult(path)(TypeTagged(doubleValue.toFloatExact), {
          case e: ArithmeticException =>
            DeserializationError.Malformed(e, reportedBy = self)
        })

      case JsonLong(longValue) =>
        DeserializationResult(path)(TypeTagged(longValue.toFloatExact), {
          case e: ArithmeticException =>
            DeserializationError.Malformed(e, reportedBy = self)
        })

      case _ =>
        DeserializationFailure(path, DeserializationError.Unexpected("Expected a JSON number", reportedBy = self))
    }

}
