package co.blocke.scalajack
package typeadapter
package javaprimitives

class BoxedDoubleSerializer(doubleSerializer: Serializer[Double]) extends Serializer[java.lang.Double] {

  override def serialize[J](tagged: TypeTagged[java.lang.Double])(implicit ops: JsonOps[J], guidance: SerializationGuidance): SerializationResult[J] =
    tagged match {
      case TypeTagged(null)  => SerializationSuccess(JsonNull())
      case TypeTagged(boxed) => doubleSerializer.serialize(TypeTagged(boxed.doubleValue))
    }

}
