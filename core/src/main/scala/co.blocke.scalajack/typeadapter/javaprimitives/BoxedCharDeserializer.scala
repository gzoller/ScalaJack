package co.blocke.scalajack
package typeadapter
package javaprimitives

import co.blocke.scalajack.typeadapter.javaprimitives.BoxedCharDeserializer.BoxedCharType

import scala.reflect.runtime.universe.{ Type, typeOf }

object BoxedCharDeserializer {

  private val BoxedCharType: Type = typeOf[java.lang.Character]

}

class BoxedCharDeserializer(charDeserializer: Deserializer[Char]) extends Deserializer[java.lang.Character] {

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
