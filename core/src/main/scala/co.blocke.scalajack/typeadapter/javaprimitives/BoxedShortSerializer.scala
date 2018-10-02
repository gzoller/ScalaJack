package co.blocke.scalajack
package typeadapter
package javaprimitives

class BoxedShortSerializer(shortSerializer: Serializer[Short]) extends Serializer[java.lang.Short] {

  override def serialize[J](tagged: TypeTagged[java.lang.Short])(implicit ops: JsonOps[J], guidance: SerializationGuidance): SerializationResult[J] =
    tagged match {
      case TypeTagged(null)  => SerializationSuccess(JsonNull())
      case TypeTagged(boxed) => shortSerializer.serialize(TypeTagged(boxed.shortValue))
    }

}
