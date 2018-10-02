package co.blocke.scalajack
package typeadapter

class EnumerationValueSerializer[E <: Enumeration] extends Serializer[E#Value] {

  override def serialize[J](tagged: TypeTagged[E#Value])(implicit ops: JsonOps[J], guidance: SerializationGuidance): SerializationResult[J] =
    tagged match {
      case TypeTagged(null)             => SerializationSuccess(JsonNull())
      case TypeTagged(enumerationValue) => SerializationSuccess(JsonString(enumerationValue.toString))
    }

}
