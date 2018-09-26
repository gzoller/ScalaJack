package co.blocke.scalajack
package typeadapter
package javaprimitives

class BoxedFloatDeserializer(floatDeserializer: Deserializer[Float]) extends Deserializer[java.lang.Float] {

  private val BoxedFloatType: Type = typeOf[java.lang.Float]

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J], guidance: DeserializationGuidance): DeserializationResult[java.lang.Float] =
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
