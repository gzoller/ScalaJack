package co.blocke.scalajack
package typeadapter

class EnumerationValueSerializer[E <: Enumeration] extends Serializer[E#Value] {

  override def serialize[AST, S](tagged: TypeTagged[E#Value])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(null)             => SerializationSuccess(AstNull())
      case TypeTagged(enumerationValue) => SerializationSuccess(AstString(enumerationValue.toString))
    }

}
