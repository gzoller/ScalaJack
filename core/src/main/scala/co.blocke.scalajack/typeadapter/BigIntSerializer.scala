package co.blocke.scalajack
package typeadapter

class BigIntSerializer extends Serializer[BigInt] {

  override def serialize[AST, S](tagged: TypeTagged[BigInt])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(null)   => SerializationSuccess(AstNull())
      case TypeTagged(bigInt) => SerializationSuccess(AstInt(bigInt))
    }

}
