package co.blocke.scalajack
package typeadapter

class IntSerializer extends Serializer[Int] {

  override def serialize[AST, S](tagged: TypeTagged[Int])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTaggedInt(intValue) => SerializationSuccess(AstLong(intValue))
      case TypeTagged(intValue)    => SerializationSuccess(AstLong(intValue))
    }

}
