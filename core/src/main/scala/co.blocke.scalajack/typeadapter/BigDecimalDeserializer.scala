package co.blocke.scalajack
package typeadapter

class BigDecimalDeserializer extends Deserializer[BigDecimal] {

  self =>

  private val BigDecimalType: Type = typeOf[BigDecimal]
  private val taggedNull: TypeTagged[BigDecimal] = TypeTagged[BigDecimal](null, BigDecimalType)

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[BigDecimal] =
    ast match {
      case AstNull()                           => DeserializationSuccess(taggedNull)
      case AstDecimal(x)                       => DeserializationSuccess(TypeTagged(x, BigDecimalType))
      case AstDouble(x)                        => DeserializationResult(path)(TypeTagged(BigDecimal(x), BigDecimalType))
      case AstInt(x)                           => DeserializationResult(path)(TypeTagged(BigDecimal(x), BigDecimalType))
      case AstLong(x)                          => DeserializationResult(path)(TypeTagged(BigDecimal(x), BigDecimalType))
      case AstString(s) if (guidance.isMapKey) => this.deserialize(path, ops.parse(s.asInstanceOf[S]))(ops, guidance = guidance.copy(isMapKey = false))
      case _                                   => DeserializationFailure(path, DeserializationError.Unexpected(s"Expected a JSON number, not $ast", reportedBy = self))
    }
}
