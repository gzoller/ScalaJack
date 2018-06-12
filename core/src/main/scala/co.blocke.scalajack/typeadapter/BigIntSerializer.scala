package co.blocke.scalajack
package typeadapter

class BigIntSerializer extends Serializer[BigInt] {

  override def serialize[J](tagged: TypeTagged[BigInt])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(null)   => SerializationSuccess(JsonNull())
      case TypeTagged(bigInt) => SerializationSuccess(JsonInt(bigInt))
    }

}
