package co.blocke.scalajack
package typeadapter
package javaprimitives

class BoxedFloatSerializer(floatSerializer: Serializer[Float]) extends Serializer[java.lang.Float] {

  override def serialize[J](tagged: TypeTagged[java.lang.Float])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(null)  => SerializationSuccess(JsonNull())
      case TypeTagged(boxed) => floatSerializer.serialize(TypeTagged(boxed.floatValue))
    }

}
