package co.blocke.scalajack
package typeadapter
package javaprimitives

class BoxedIntSerializer(intSerializer: Serializer[Int]) extends Serializer[java.lang.Integer] {

  override def serialize[AST, S](tagged: TypeTagged[java.lang.Integer])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(null)  => SerializationSuccess(AstNull())
      case TypeTagged(boxed) => intSerializer.serialize(TypeTagged(boxed.intValue))
    }

}
