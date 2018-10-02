package co.blocke.scalajack
package typeadapter

class ShortSerializer extends Serializer[Short] {

  override def serialize[J](tagged: TypeTagged[Short])(implicit ops: JsonOps[J], guidance: SerializationGuidance): SerializationResult[J] =
    tagged match {
      case TypeTaggedShort(shortValue) => SerializationSuccess(JsonLong(shortValue.toLong))
      case TypeTagged(shortValue)      => SerializationSuccess(JsonLong(shortValue.toLong))
    }

}
