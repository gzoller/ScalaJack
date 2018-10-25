package co.blocke.scalajack
package typeadapter

class StringSerializer extends Serializer[String] {

  override def serialize[AST, S](tagged: TypeTagged[String])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(null)  => SerializationSuccess(AstNull())
      case TypeTagged(value) => SerializationSuccess(AstString(value))
    }

}
