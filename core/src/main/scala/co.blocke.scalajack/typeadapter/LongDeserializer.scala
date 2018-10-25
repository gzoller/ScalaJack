package co.blocke.scalajack
package typeadapter

class LongDeserializer extends Deserializer[Long] {

  self =>

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[Long] =
    ast match {
      case AstLong(x)                          => DeserializationSuccess(TypeTagged(x))
      case AstString(s) if (guidance.isMapKey) => this.deserialize(path, ops.parse(s.asInstanceOf[S]))(ops, guidance = guidance.copy(isMapKey = false))
      case _ =>
        DeserializationFailure(path, DeserializationError.Unexpected("Expected a JSON number (long)", reportedBy = self))
    }

}
