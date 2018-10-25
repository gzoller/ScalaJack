package co.blocke.scalajack
package typeadapter
package javaprimitives

class BoxedCharSerializer(charSerializer: Serializer[Char]) extends Serializer[java.lang.Character] {

  override def serialize[AST, S](tagged: TypeTagged[Character])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(null)  => SerializationSuccess(AstNull())
      case TypeTagged(boxed) => charSerializer.serialize(TypeTagged(boxed.charValue))
    }

}
