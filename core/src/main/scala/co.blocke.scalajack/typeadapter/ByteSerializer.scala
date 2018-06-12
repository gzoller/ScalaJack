package co.blocke.scalajack
package typeadapter

class ByteSerializer extends Serializer[Byte] {

  override def serialize[J](tagged: TypeTagged[Byte])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTaggedByte(byteValue) => SerializationSuccess(JsonLong(byteValue.longValue))
      case TypeTagged(byteValue)     => SerializationSuccess(JsonLong(byteValue.longValue))
    }

}
