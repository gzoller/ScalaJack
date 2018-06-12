package co.blocke.scalajack
package typeadapter

class BigDecimalDeserializer extends Deserializer[BigDecimal] {

  private val BigDecimalType: Type = typeOf[BigDecimal]

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[BigDecimal] =
    json match {
      case JsonInt(x)  => DeserializationResult(path)(TypeTagged(BigDecimal(x), BigDecimalType))
      case JsonLong(x) => DeserializationResult(path)(TypeTagged(BigDecimal(x), BigDecimalType))
      case _           => DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON number"))
    }

}
