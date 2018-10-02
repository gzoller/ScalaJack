package co.blocke.scalajack
package typeadapter
package javaprimitives

class BoxedBooleanSerializer(booleanSerializer: Serializer[Boolean]) extends Serializer[java.lang.Boolean] {

  override def serialize[J](tagged: TypeTagged[java.lang.Boolean])(implicit ops: JsonOps[J], guidance: SerializationGuidance): SerializationResult[J] =
    tagged match {
      case TypeTagged(null)  => SerializationSuccess(JsonNull())
      case TypeTagged(boxed) => booleanSerializer.serialize(TypeTagged(boxed.booleanValue))
    }

}
