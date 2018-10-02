package co.blocke.scalajack
package typeadapter
package javaprimitives

class BoxedLongDeserializer(longDeserializer: Deserializer[Long]) extends Deserializer[java.lang.Long] {

  private val BoxedLongType: Type = typeOf[java.lang.Long]

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J], guidance: SerializationGuidance): DeserializationResult[java.lang.Long] =
    json match {
      case JsonNull() =>
        DeserializationSuccess(TypeTagged(null, BoxedLongType))

      case _ =>
        longDeserializer.deserialize(path, json) map {
          case TypeTaggedLong(longValue) => TypeTagged(java.lang.Long.valueOf(longValue), BoxedLongType)
          case TypeTagged(longValue)     => TypeTagged(java.lang.Long.valueOf(longValue), BoxedLongType)
        }
    }

}
