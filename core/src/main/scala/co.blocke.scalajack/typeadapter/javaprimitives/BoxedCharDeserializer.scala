package co.blocke.scalajack
package typeadapter
package javaprimitives

class BoxedCharDeserializer(charDeserializer: Deserializer[Char]) extends Deserializer[java.lang.Character] {

  private val BoxedCharType: Type = typeOf[java.lang.Character]

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J], guidance: DeserializationGuidance): DeserializationResult[java.lang.Character] =
    json match {
      case JsonNull() =>
        DeserializationSuccess(TypeTagged(null, BoxedCharType))

      case _ =>
        charDeserializer.deserialize(path, json) map {
          case TypeTaggedChar(charValue) => TypeTagged(java.lang.Character.valueOf(charValue), BoxedCharType)
          case TypeTagged(charValue)     => TypeTagged(java.lang.Character.valueOf(charValue), BoxedCharType)
        }
    }

}
