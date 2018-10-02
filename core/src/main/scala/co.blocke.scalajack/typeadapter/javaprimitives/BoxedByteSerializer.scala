package co.blocke.scalajack
package typeadapter
package javaprimitives

class BoxedByteSerializer(byteSerializer: Serializer[Byte]) extends Serializer[java.lang.Byte] {

  override def serialize[J](tagged: TypeTagged[java.lang.Byte])(implicit ops: JsonOps[J], guidance: SerializationGuidance): SerializationResult[J] =
    tagged match {
      case TypeTagged(null)  => SerializationSuccess(JsonNull())
      case TypeTagged(boxed) => byteSerializer.serialize(TypeTagged(boxed.byteValue))
    }

}
