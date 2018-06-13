package co.blocke.scalajack
package typeadapter

class IntSerializer extends Serializer[Int] {

  override def serialize[J](tagged: TypeTagged[Int])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTaggedInt(intValue) => SerializationSuccess(JsonLong(intValue))
    }

}
