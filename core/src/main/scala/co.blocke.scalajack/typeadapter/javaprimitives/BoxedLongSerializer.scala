package co.blocke.scalajack
package typeadapter
package javaprimitives

class BoxedLongSerializer(longSerializer: Serializer[Long]) extends Serializer[java.lang.Long] {

  override def serialize[J](tagged: TypeTagged[java.lang.Long])(implicit ops: JsonOps[J], guidance: SerializationGuidance): SerializationResult[J] =
    tagged match {
      case TypeTagged(null)  => SerializationSuccess(JsonNull())
      case TypeTagged(boxed) => longSerializer.serialize(TypeTagged(boxed.longValue))
    }

}
