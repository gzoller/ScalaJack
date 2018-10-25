package co.blocke.scalajack
package typeadapter
package javaprimitives

class BoxedByteSerializer(byteSerializer: Serializer[Byte]) extends Serializer[java.lang.Byte] {

  override def serialize[AST, S](tagged: TypeTagged[java.lang.Byte])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(null)  => SerializationSuccess(AstNull())
      case TypeTagged(boxed) => byteSerializer.serialize(TypeTagged(boxed.byteValue))
    }

}
