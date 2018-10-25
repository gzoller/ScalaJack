package co.blocke.scalajack
package typeadapter

class BooleanSerializer extends Serializer[Boolean] {

  override def serialize[AST, S](tagged: TypeTagged[Boolean])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTaggedBoolean(booleanValue) => SerializationSuccess(AstBoolean(booleanValue))
      case TypeTagged(booleanValue)        => SerializationSuccess(AstBoolean(booleanValue))
    }

}
