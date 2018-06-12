package co.blocke.scalajack
package typeadapter
package javaprimitives

class BoxedIntSerializer(intSerializer: Serializer[Int]) extends Serializer[java.lang.Integer] {

  override def serialize[J](tagged: TypeTagged[java.lang.Integer])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(null)  => SerializationSuccess(JsonNull())
      case TypeTagged(boxed) => intSerializer.serialize(TypeTagged(boxed.intValue))
    }

}
