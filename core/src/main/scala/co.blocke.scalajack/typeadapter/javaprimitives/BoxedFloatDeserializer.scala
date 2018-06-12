package co.blocke.scalajack
package typeadapter
package javaprimitives

import co.blocke.scalajack.typeadapter.javaprimitives.BoxedFloatDeserializer.BoxedFloatType

import scala.reflect.runtime.universe.{ Type, typeOf }

object BoxedFloatDeserializer {

  private val BoxedFloatType: Type = typeOf[java.lang.Float]

}

class BoxedFloatDeserializer(floatDeserializer: Deserializer[Float]) extends Deserializer[java.lang.Float] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[java.lang.Float] =
    json match {
      case JsonNull() =>
        DeserializationSuccess(TypeTagged(null, BoxedFloatType))

      case _ =>
        floatDeserializer.deserialize(path, json) map {
          case TypeTaggedFloat(floatValue) => TypeTagged(java.lang.Float.valueOf(floatValue), BoxedFloatType)
          case TypeTagged(floatValue)      => TypeTagged(java.lang.Float.valueOf(floatValue), BoxedFloatType)
        }
    }

}
