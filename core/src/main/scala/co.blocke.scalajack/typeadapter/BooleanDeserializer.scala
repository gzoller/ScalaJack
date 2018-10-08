package co.blocke.scalajack
package typeadapter

class BooleanDeserializer extends Deserializer[Boolean] {

  self =>

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J], guidance: SerializationGuidance): DeserializationResult[Boolean] =
    json match {
      case JsonBoolean(booleanValue)            => DeserializationSuccess(TypeTagged(booleanValue))
      case JsonString(s) if (guidance.isMapKey) => this.deserialize(path, ops.parse(s))(ops, guidance = guidance.copy(isMapKey = false))
      case _                                    => DeserializationFailure(path, DeserializationError.Unexpected("Expected a JSON boolean", reportedBy = self))
    }

}
