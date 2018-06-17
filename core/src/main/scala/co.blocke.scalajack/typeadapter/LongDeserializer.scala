package co.blocke.scalajack
package typeadapter

class LongDeserializer extends Deserializer[Long] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[Long] =
    json match {
      case JsonLong(x) => DeserializationSuccess(TypeTagged(x))
      case _           => DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON number"))
    }

}