package co.blocke.scalajack
package typeadapter

class LongDeserializer extends Deserializer[Long] {

  self =>

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J], guidance: DeserializationGuidance): DeserializationResult[Long] =
    json match {
      case JsonLong(x) => DeserializationSuccess(TypeTagged(x))
      case _           => DeserializationFailure(path, DeserializationError.Unexpected("Expected a JSON number", reportedBy = self))
    }

}
