package co.blocke.scalajack
package typeadapter
package javaprimitives

object BoxedCharDeserializer {

  private val BoxedCharType: Type = typeOf[java.lang.Character]

}

class BoxedCharDeserializer(charDeserializer: Deserializer[Char]) extends Deserializer[java.lang.Character] {

  import BoxedCharDeserializer.BoxedCharType

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[java.lang.Character] =
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
