package co.blocke.scalajack
package typeadapter

class ShortDeserializer extends Deserializer[Short] {

  self =>

  import NumberConverters._

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J], guidance: SerializationGuidance): DeserializationResult[Short] =
    json match {
      case JsonLong(longValue) if (longValue >= -32768 && longValue <= 32767) => DeserializationSuccess(TypeTagged(longValue.toShortExact))
      case JsonLong(_) => DeserializationFailure(path, DeserializationError.Unexpected("Short value out of range", reportedBy = self))
      case JsonInt(bigInt) if (bigInt >= -32768 && bigInt <= 32767) => DeserializationSuccess(TypeTagged(bigInt.toShortExact))
      case JsonInt(_) => DeserializationFailure(path, DeserializationError.Unexpected("Short value out of range", reportedBy = self))
      case JsonString(s) if (guidance.isMapKey) => this.deserialize(path, ops.parse(s))(ops, guidance = guidance.copy(isMapKey = false))
      case _ => DeserializationFailure(path, DeserializationError.Unexpected(s"Expected a JSON number (short), not $json", reportedBy = self))
    }
}
