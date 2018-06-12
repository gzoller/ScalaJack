package co.blocke.scalajack
package typeadapter
package javaprimitives

object BoxedBooleanDeserializer {

  private val BoxedBooleanType: Type = typeOf[java.lang.Boolean]

}

class BoxedBooleanDeserializer(booleanDeserializer: Deserializer[Boolean]) extends Deserializer[java.lang.Boolean] {

  import BoxedBooleanDeserializer.BoxedBooleanType

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
