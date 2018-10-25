package co.blocke.scalajack
package typeadapter

class ShortDeserializer extends Deserializer[Short] {

  self =>

  import NumberConverters._

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[Short] =
    ast match {
      case AstLong(longValue) if (longValue >= -32768 && longValue <= 32767) => DeserializationSuccess(TypeTagged(longValue.toShortExact))
      case AstLong(_) => DeserializationFailure(path, DeserializationError.Unexpected("Short value out of range", reportedBy = self))
      case AstInt(bigInt) if (bigInt >= -32768 && bigInt <= 32767) => DeserializationSuccess(TypeTagged(bigInt.toShortExact))
      case AstInt(_) => DeserializationFailure(path, DeserializationError.Unexpected("Short value out of range", reportedBy = self))
      case AstString(s) if (guidance.isMapKey) => this.deserialize(path, ops.parse(s.asInstanceOf[S]))(ops, guidance = guidance.copy(isMapKey = false))
      case _ => DeserializationFailure(path, DeserializationError.Unexpected(s"Expected a JSON number (short), not $ast", reportedBy = self))
    }
}
