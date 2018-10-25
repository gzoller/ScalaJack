package co.blocke.scalajack
package typeadapter

class ByteSerializer extends Serializer[Byte] {

  override def serialize[AST, S](tagged: TypeTagged[Byte])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTaggedByte(byteValue) => SerializationSuccess(AstLong(byteValue.longValue))
      case TypeTagged(byteValue)     => SerializationSuccess(AstLong(byteValue.longValue))
    }

}
