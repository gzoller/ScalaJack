package co.blocke.scalajack
package typeadapter

class CaseObjectSerializer[C]()(implicit tt: TypeTag[C]) extends Serializer[C] {

  def serialize[AST, S](tagged: TypeTagged[C])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(AstNull())
      case TypeTagged(c)    => SerializationSuccess(AstString(c.toString))
    }

}
