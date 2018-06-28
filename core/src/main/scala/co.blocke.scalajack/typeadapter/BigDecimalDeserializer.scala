package co.blocke.scalajack
package typeadapter

class BigDecimalDeserializer extends Deserializer[BigDecimal] {

  self =>

  private val BigDecimalType: Type = typeOf[BigDecimal]
  private val taggedNull: TypeTagged[BigDecimal] = TypeTagged[BigDecimal](null, BigDecimalType)

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[BigDecimal] =
    json match {
      case JsonNull()     => DeserializationSuccess(taggedNull)
      case JsonDecimal(x) => DeserializationSuccess(TypeTagged(x, BigDecimalType))
      case JsonDouble(x)  => DeserializationResult(path)(TypeTagged(BigDecimal(x), BigDecimalType))
      case JsonInt(x)     => DeserializationResult(path)(TypeTagged(BigDecimal(x), BigDecimalType))
      case JsonLong(x)    => DeserializationResult(path)(TypeTagged(BigDecimal(x), BigDecimalType))
      case _              => DeserializationFailure(path, DeserializationError.Unsupported(s"Expected a JSON number, not $json", reportedBy = self))
    }

}
