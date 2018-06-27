package co.blocke.scalajack
package typeadapter

class ByteDeserializer extends Deserializer[Byte] {

  self =>

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[Byte] =
    json match {
      case JsonLong(longValue) => DeserializationSuccess(TypeTagged(longValue.byteValue))
      case _                   => DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON number", reportedBy = Some(self)))
    }

}
