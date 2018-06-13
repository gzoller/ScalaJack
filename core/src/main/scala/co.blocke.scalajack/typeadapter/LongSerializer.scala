package co.blocke.scalajack
package typeadapter

class LongSerializer extends Serializer[Long] {

  override def serialize[J](tagged: TypeTagged[Long])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(x) => SerializationSuccess(JsonLong(x))
    }

}
