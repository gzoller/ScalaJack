package co.blocke.scalajack
package typeadapter

class LongDeserializer extends Deserializer[Long] {

  self =>

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J], guidance: SerializationGuidance): DeserializationResult[Long] =
    json match {
      case JsonLong(x)                          => DeserializationSuccess(TypeTagged(x))
      case JsonString(s) if (guidance.isMapKey) => this.deserialize(path, ops.parse(s))(ops, guidance = guidance.copy(isMapKey = false))
      case _ =>
        DeserializationFailure(path, DeserializationError.Unexpected("Expected a JSON number (long)", reportedBy = self))
    }

}
