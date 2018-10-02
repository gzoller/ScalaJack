package co.blocke.scalajack
package typeadapter

class CharSerializer extends Serializer[Char] {

  override def serialize[J](tagged: TypeTagged[Char])(implicit ops: JsonOps[J], guidance: SerializationGuidance): SerializationResult[J] =
    tagged match {
      case TypeTaggedChar(charValue) => SerializationSuccess(JsonString("" + charValue))
      case TypeTagged(charValue)     => SerializationSuccess(JsonString("" + charValue))
    }

}
