package co.blocke.scalajack
package typeadapter
package javaprimitives

import co.blocke.scalajack.typeadapter.javaprimitives.BoxedBooleanDeserializer.BoxedBooleanType

import scala.reflect.runtime.universe.{ Type, typeOf }

object BoxedBooleanDeserializer {

  val BoxedBooleanType: Type = typeOf[java.lang.Boolean]

}

class BoxedBooleanDeserializer(booleanDeserializer: Deserializer[Boolean]) extends Deserializer[java.lang.Boolean] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[java.lang.Boolean] =
    json match {
      case JsonNull() =>
        DeserializationSuccess(TypeTagged(null, BoxedBooleanType))

      case _ =>
        booleanDeserializer.deserialize(path, json) map {
          case TypeTaggedBoolean(booleanValue) => TypeTagged(java.lang.Boolean.valueOf(booleanValue), BoxedBooleanType)
          case TypeTagged(booleanValue)        => TypeTagged(java.lang.Boolean.valueOf(booleanValue), BoxedBooleanType)
        }
    }

}
