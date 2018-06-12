package co.blocke.scalajack
package typeadapter
package javaprimitives

class BoxedShortDeserializer(shortDeserializer: Deserializer[Short]) extends Deserializer[java.lang.Short] {

  private val BoxedShortType: Type = typeOf[java.lang.Short]

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[java.lang.Short] =
    json match {
      case JsonNull() =>
        DeserializationSuccess(TypeTagged(null, BoxedShortType))

      case _ =>
        shortDeserializer.deserialize(path, json) map {
          case TypeTaggedShort(shortValue) => TypeTagged(java.lang.Short.valueOf(shortValue), BoxedShortType)
          case TypeTagged(shortValue)      => TypeTagged(java.lang.Short.valueOf(shortValue), BoxedShortType)
        }
    }

}
