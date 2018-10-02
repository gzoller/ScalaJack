package co.blocke.scalajack
package typeadapter

class BigDecimalSerializer extends Serializer[BigDecimal] {

  override def serialize[J](tagged: TypeTagged[BigDecimal])(implicit ops: JsonOps[J], guidance: SerializationGuidance): SerializationResult[J] =
    tagged match {
      case TypeTagged(null)       => SerializationSuccess(JsonNull())
      case TypeTagged(bigDecimal) => SerializationSuccess(JsonDecimal(bigDecimal))
    }

}
