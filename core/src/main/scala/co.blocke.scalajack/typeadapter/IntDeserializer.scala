package co.blocke.scalajack
package typeadapter

class IntDeserializer extends Deserializer[Int] {

  self =>

  import NumberConverters._

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J], guidance: SerializationGuidance): DeserializationResult[Int] =
    json match {
      case JsonLong(longValue) => DeserializationResult(path)(TypeTagged(longValue.toIntExact))
      case _                   => DeserializationFailure(path, DeserializationError.Unexpected(s"Expected a JSON int, not $json", reportedBy = self))
    }

}
