package co.blocke.scalajack
package typeadapter
package javaprimitives

class BoxedDoubleDeserializer(doubleDeserializer: Deserializer[Double]) extends Deserializer[java.lang.Double] {

  private val BoxedDoubleType: Type = typeOf[java.lang.Double]

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J], guidance: DeserializationGuidance): DeserializationResult[java.lang.Double] =
    json match {
      case JsonNull() =>
        DeserializationSuccess(TypeTagged(null, BoxedDoubleType))

      case _ =>
        doubleDeserializer.deserialize(path, json) map {
          case TypeTaggedDouble(doubleValue) => TypeTagged(java.lang.Double.valueOf(doubleValue), BoxedDoubleType)
          case TypeTagged(doubleValue)       => TypeTagged(java.lang.Double.valueOf(doubleValue), BoxedDoubleType)
        }
    }

}
