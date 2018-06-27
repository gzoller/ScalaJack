package co.blocke.scalajack
package typeadapter

class DoubleDeserializer extends Deserializer[Double] {

  self =>

  import NumberConverters._

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[Double] =
    json match {
      case JsonDecimal(x)          => DeserializationResult(path)(TypeTagged(x.toDoubleExact))
      case JsonDouble(doubleValue) => DeserializationSuccess(TypeTagged(doubleValue))
      // TODO handle other JSON types
      case _                       => DeserializationFailure(path, DeserializationError.Unsupported(s"Expected a JSON number, not $json", reportedBy = Some(self)))
    }

}
