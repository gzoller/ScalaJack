package co.blocke.scalajack
package typeadapter
package javaprimitives

class BoxedShortSerializer(shortSerializer: Serializer[Short]) extends Serializer[java.lang.Short] {

  override def serialize[AST, S](tagged: TypeTagged[java.lang.Short])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(null)  => SerializationSuccess(AstNull())
      case TypeTagged(boxed) => shortSerializer.serialize(TypeTagged(boxed.shortValue))
    }

}
