package co.blocke.scalajack
package typeadapter
package javaprimitives

import co.blocke.scalajack.typeadapter.javaprimitives.BoxedIntDeserializer.BoxedIntType

import scala.reflect.runtime.universe.{ Type, typeOf }

object BoxedIntDeserializer {

  private val BoxedIntType: Type = typeOf[java.lang.Integer]

}

class BoxedIntDeserializer(intDeserializer: Deserializer[Int]) extends Deserializer[java.lang.Integer] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[java.lang.Integer] =
    json match {
      case JsonNull() =>
        DeserializationSuccess(TypeTagged(null, BoxedIntType))

      case _ =>
        intDeserializer.deserialize(path, json) map {
          case TypeTaggedInt(intValue) => TypeTagged(java.lang.Integer.valueOf(intValue), BoxedIntType)
          case TypeTagged(intValue)    => TypeTagged(java.lang.Integer.valueOf(intValue), BoxedIntType)
        }
    }

}
