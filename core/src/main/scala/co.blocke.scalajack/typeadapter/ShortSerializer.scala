package co.blocke.scalajack
package typeadapter

class ShortSerializer extends Serializer[Short] {

  override def serialize[AST, S](tagged: TypeTagged[Short])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTaggedShort(shortValue) => SerializationSuccess(AstInt(shortValue.toInt))
      case TypeTagged(shortValue)      => SerializationSuccess(AstInt(shortValue.toInt))
    }

}
