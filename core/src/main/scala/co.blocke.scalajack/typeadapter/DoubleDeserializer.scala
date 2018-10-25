package co.blocke.scalajack
package typeadapter

class DoubleDeserializer extends Deserializer[Double] {

  self =>

  import NumberConverters._

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[Double] =
    ast match {
      case AstDecimal(x)                       => DeserializationResult(path)(TypeTagged(x.toDoubleExact))
      case AstDouble(doubleValue)              => DeserializationSuccess(TypeTagged(doubleValue))
      case AstString(s) if (guidance.isMapKey) => this.deserialize(path, ops.parse(s.asInstanceOf[S]))(ops, guidance = guidance.copy(isMapKey = false))
      // TODO handle other JSON types
      case _                                   => DeserializationFailure(path, DeserializationError.Unexpected(s"Expected a JSON number, not $ast", reportedBy = self))
    }

}
