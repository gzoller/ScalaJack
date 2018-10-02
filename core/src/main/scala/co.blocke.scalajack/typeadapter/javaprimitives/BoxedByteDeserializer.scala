package co.blocke.scalajack
package typeadapter
package javaprimitives

class BoxedByteDeserializer(byteDeserializer: Deserializer[Byte]) extends Deserializer[java.lang.Byte] {

  private val BoxedByteType: Type = typeOf[java.lang.Byte]

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J], guidance: SerializationGuidance): DeserializationResult[java.lang.Byte] =
    json match {
      case JsonNull() =>
        DeserializationSuccess(TypeTagged(null, BoxedByteType))

      case _ =>
        byteDeserializer.deserialize(path, json) map {
          case TypeTaggedByte(byteValue) => TypeTagged(java.lang.Byte.valueOf(byteValue), BoxedByteType)
          case TypeTagged(byteValue)     => TypeTagged(java.lang.Byte.valueOf(byteValue), BoxedByteType)
        }
    }

}
