package co.blocke.scalajack
package typeadapter
package javaprimitives

class BoxedCharSerializer(charSerializer: Serializer[Char]) extends Serializer[java.lang.Character] {

  override def serialize[J](tagged: TypeTagged[Character])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(null)  => SerializationSuccess(JsonNull())
      case TypeTagged(boxed) => charSerializer.serialize(TypeTagged(boxed.charValue))
    }

}
