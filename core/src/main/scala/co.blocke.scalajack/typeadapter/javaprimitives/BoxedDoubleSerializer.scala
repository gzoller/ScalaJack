package co.blocke.scalajack
package typeadapter
package javaprimitives

class BoxedDoubleSerializer(doubleSerializer: Serializer[Double]) extends Serializer[java.lang.Double] {

  override def serialize[AST, S](tagged: TypeTagged[java.lang.Double])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(null)  => SerializationSuccess(AstNull())
      case TypeTagged(boxed) => doubleSerializer.serialize(TypeTagged(boxed.doubleValue))
    }

}
