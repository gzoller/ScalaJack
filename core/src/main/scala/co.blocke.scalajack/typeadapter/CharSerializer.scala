package co.blocke.scalajack
package typeadapter

class CharSerializer extends Serializer[Char] {

  override def serialize[AST, S](tagged: TypeTagged[Char])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTaggedChar(charValue) => SerializationSuccess(AstString("" + charValue))
      case TypeTagged(charValue)     => SerializationSuccess(AstString("" + charValue))
    }

}
