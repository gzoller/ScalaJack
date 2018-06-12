package co.blocke.scalajack
package typeadapter

class FloatDeserializer extends Deserializer[Float] {

  import NumberConverters._

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[Float] =
    json match {
      case JsonDecimal(bigDecimal) =>
        ??? // TODO

      case JsonDouble(doubleValue) =>
        DeserializationResult(path)(TypeTagged(doubleValue.toFloatExact), {
          case e: ArithmeticException =>
            DeserializationError.Malformed(e)
        })

      case JsonLong(longValue) =>
        DeserializationResult(path)(TypeTagged(longValue.toFloatExact), {
          case e: ArithmeticException =>
            DeserializationError.Malformed(e)
        })

      case _ =>
        DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON number"))
    }

}
