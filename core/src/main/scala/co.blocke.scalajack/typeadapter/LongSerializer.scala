package co.blocke.scalajack
package typeadapter

class LongSerializer extends Serializer[Long] {

  override def serialize[AST, S](tagged: TypeTagged[Long])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTaggedLong(longValue) => SerializationSuccess(AstLong(longValue))
      case TypeTagged(longValue)     => SerializationSuccess(AstLong(longValue))
    }

}
