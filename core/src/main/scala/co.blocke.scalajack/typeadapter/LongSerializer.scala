package co.blocke.scalajack
package typeadapter

class LongSerializer extends Serializer[Long] {

  override def serialize[J](tagged: TypeTagged[Long])(implicit ops: JsonOps[J], guidance: SerializationGuidance): SerializationResult[J] =
    tagged match {
      case TypeTaggedLong(longValue) => SerializationSuccess(JsonLong(longValue))
      case TypeTagged(longValue)     => SerializationSuccess(JsonLong(longValue))
    }

}
