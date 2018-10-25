package co.blocke.scalajack
package typeadapter

class BigDecimalSerializer extends Serializer[BigDecimal] {

  override def serialize[AST, S](tagged: TypeTagged[BigDecimal])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(null)       => SerializationSuccess(AstNull())
      case TypeTagged(bigDecimal) => SerializationSuccess(AstDecimal(bigDecimal))
    }

}
