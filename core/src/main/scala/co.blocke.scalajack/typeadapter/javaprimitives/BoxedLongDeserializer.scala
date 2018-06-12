package co.blocke.scalajack
package typeadapter
package javaprimitives

object BoxedLongDeserializer {

  private val BoxedLongType: Type = typeOf[java.lang.Long]

}

class BoxedLongDeserializer(longDeserializer: Deserializer[Long]) extends Deserializer[java.lang.Long] {

  import BoxedLongDeserializer.BoxedLongType

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[java.lang.Long] =
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
