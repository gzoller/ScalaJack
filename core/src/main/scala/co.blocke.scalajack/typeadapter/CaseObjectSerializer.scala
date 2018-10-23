package co.blocke.scalajack
package typeadapter

class CaseObjectSerializer[C]()(implicit tt: TypeTag[C]) extends Serializer[C] {

  def serialize[J](tagged: TypeTagged[C])(implicit ops: JsonOps[J], guidance: SerializationGuidance): SerializationResult[J] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(JsonNull())
      case TypeTagged(c)    => SerializationSuccess(JsonString(c.toString))
    }

}
