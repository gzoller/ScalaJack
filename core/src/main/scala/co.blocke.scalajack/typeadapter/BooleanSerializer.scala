package co.blocke.scalajack
package typeadapter

class BooleanSerializer extends Serializer[Boolean] {

  override def serialize[J](tagged: TypeTagged[Boolean])(implicit ops: JsonOps[J], guidance: SerializationGuidance): SerializationResult[J] =
    tagged match {
      case TypeTaggedBoolean(booleanValue) => SerializationSuccess(JsonBoolean(booleanValue))
      case TypeTagged(booleanValue)        => SerializationSuccess(JsonBoolean(booleanValue))
    }

}
