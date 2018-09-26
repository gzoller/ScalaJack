package co.blocke.scalajack
package typeadapter

class ByteDeserializer extends Deserializer[Byte] {

  self =>

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J], guidance: DeserializationGuidance): DeserializationResult[Byte] =
    json match {
      case JsonLong(longValue) if (longValue >= -128 && longValue <= 127) => DeserializationSuccess(TypeTagged(longValue.byteValue))
      case JsonLong(longValue) => DeserializationFailure(path, DeserializationError.Unexpected("Byte value out of range", reportedBy = self))
      case _ => DeserializationFailure(path, DeserializationError.Unexpected("Expected a JSON number", reportedBy = self))
    }

}
