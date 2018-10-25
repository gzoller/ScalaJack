package co.blocke.scalajack
package typeadapter
package javaprimitives

class BoxedLongSerializer(longSerializer: Serializer[Long]) extends Serializer[java.lang.Long] {

  override def serialize[AST, S](tagged: TypeTagged[java.lang.Long])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(null)  => SerializationSuccess(AstNull())
      case TypeTagged(boxed) => longSerializer.serialize(TypeTagged(boxed.longValue))
    }

}
