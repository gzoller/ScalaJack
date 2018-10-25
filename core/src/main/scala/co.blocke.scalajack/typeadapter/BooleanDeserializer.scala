package co.blocke.scalajack
package typeadapter

class BooleanDeserializer extends Deserializer[Boolean] {

  self =>

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[Boolean] =
    ast match {
      case AstBoolean(booleanValue)            => DeserializationSuccess(TypeTagged(booleanValue))
      case AstString(s) if (guidance.isMapKey) => this.deserialize(path, ops.parse(s.asInstanceOf[S]))(ops, guidance = guidance.copy(isMapKey = false))
      case _                                   => DeserializationFailure(path, DeserializationError.Unexpected("Expected a JSON boolean", reportedBy = self))
    }

}
