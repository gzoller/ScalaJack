package co.blocke.scalajack
package typeadapter

class ByteDeserializer extends Deserializer[Byte] {

  self =>

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J], guidance: SerializationGuidance): DeserializationResult[Byte] =
    json match {
      case JsonLong(longValue) if (longValue >= -128 && longValue <= 127) => DeserializationSuccess(TypeTagged(longValue.byteValue))
      case JsonLong(_) => DeserializationFailure(path, DeserializationError.Unexpected("Byte value out of range", reportedBy = self))
      case JsonString(s) if (guidance.isMapKey) => this.deserialize(path, ops.parse(s))(ops, guidance = guidance.copy(isMapKey = false))
      case _ =>
        DeserializationFailure(path, DeserializationError.Unexpected("Expected a JSON number (byte)", reportedBy = self))
    }

}
