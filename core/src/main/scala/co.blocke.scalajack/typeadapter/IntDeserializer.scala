package co.blocke.scalajack
package typeadapter

class IntDeserializer extends Deserializer[Int] {

  self =>

  import NumberConverters._

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J], guidance: SerializationGuidance): DeserializationResult[Int] =
    json match {
      case JsonLong(longValue) if (longValue >= -2147483648 && longValue <= 2147483647) => DeserializationResult(path)(TypeTagged(longValue.toIntExact))
      case JsonLong(_) => DeserializationFailure(path, DeserializationError.Unexpected("Int value out of range", reportedBy = self))
      case JsonInt(bigInt) => DeserializationSuccess(TypeTagged(bigInt.toShortExact))
      case JsonString(s) if (guidance.isMapKey) => this.deserialize(path, ops.parse(s))(ops, guidance = guidance.copy(isMapKey = false))
      case _ => DeserializationFailure(path, DeserializationError.Unexpected(s"Expected a JSON int, not $json", reportedBy = self))
    }

}
