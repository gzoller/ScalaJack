package co.blocke.scalajack
package typeadapter
package javaprimitives

class BoxedBooleanSerializer(booleanSerializer: Serializer[Boolean]) extends Serializer[java.lang.Boolean] {

  override def serialize[AST, S](tagged: TypeTagged[java.lang.Boolean])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(null)  => SerializationSuccess(AstNull())
      case TypeTagged(boxed) => booleanSerializer.serialize(TypeTagged(boxed.booleanValue))
    }

}
