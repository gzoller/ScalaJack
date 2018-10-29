package co.blocke.scalajack
package typeadapter

class IntDeserializer extends Deserializer[Int] {

  self =>

  import NumberConverters._

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[Int] =
    ast match {
      case AstLong(longValue) if (longValue >= -2147483648 && longValue <= 2147483647) => DeserializationResult(path)(TypeTagged(longValue.toIntExact))
      case AstLong(_) => DeserializationFailure(path, DeserializationError.Unexpected("Int value out of range", reportedBy = self))
      case AstInt(bigInt) => DeserializationSuccess(TypeTagged(bigInt.intValue))
      case AstString(s) if (guidance.isMapKey) => this.deserialize(path, ops.parse(s.asInstanceOf[S]))(ops, guidance = guidance.copy(isMapKey = false))
      case _ => DeserializationFailure(path, DeserializationError.Unexpected(s"Expected a JSON int, not $ast", reportedBy = self))
    }

}
