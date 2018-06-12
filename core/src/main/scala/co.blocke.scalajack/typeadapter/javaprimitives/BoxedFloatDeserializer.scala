package co.blocke.scalajack
package typeadapter
package javaprimitives

object BoxedFloatDeserializer {

  private val BoxedFloatType: Type = typeOf[java.lang.Float]

}

class BoxedFloatDeserializer(floatDeserializer: Deserializer[Float]) extends Deserializer[java.lang.Float] {

  import BoxedFloatDeserializer.BoxedFloatType

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
