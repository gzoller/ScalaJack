package co.blocke.scalajack
package typeadapter
package javaprimitives

class BoxedFloatSerializer(floatSerializer: Serializer[Float]) extends Serializer[java.lang.Float] {

  override def serialize[AST, S](tagged: TypeTagged[java.lang.Float])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] = {
    tagged match {
      case TypeTagged(null)  => SerializationSuccess(AstNull())
      case TypeTagged(boxed) => floatSerializer.serialize(TypeTagged(boxed.floatValue))
    }
  }
}
