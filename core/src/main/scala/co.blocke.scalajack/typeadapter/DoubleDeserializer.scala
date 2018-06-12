package co.blocke.scalajack
package typeadapter

class DoubleDeserializer extends Deserializer[Double] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[Double] =
    json match {
      case JsonDouble(doubleValue) => DeserializationSuccess(TypeTagged(doubleValue))
      // TODO handle other JSON types
      case _                       => DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON number"))
    }

}
