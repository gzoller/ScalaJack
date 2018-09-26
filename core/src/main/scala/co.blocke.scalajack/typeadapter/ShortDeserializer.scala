package co.blocke.scalajack
package typeadapter

class ShortDeserializer extends Deserializer[Short] {

  self =>

  import NumberConverters._

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J], guidance: DeserializationGuidance): DeserializationResult[Short] =
    json match {
      case JsonLong(longValue) => DeserializationSuccess(TypeTagged(longValue.toShortExact))
      case JsonInt(bigInt)     => DeserializationSuccess(TypeTagged(bigInt.toShortExact))
      case _                   => DeserializationFailure(path, DeserializationError.Unexpected(s"Expected a JSON number, not $json", reportedBy = self))
    }

}
