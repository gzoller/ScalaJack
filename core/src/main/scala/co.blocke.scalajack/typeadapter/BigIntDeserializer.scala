package co.blocke.scalajack
package typeadapter

class BigIntDeserializer extends Deserializer[BigInt] {

  self =>

  private val BigIntType: Type = typeOf[BigInt]

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[BigInt] =
    ast match {
      case AstNull()           => DeserializationSuccess(TypeTagged(null, BigIntType))
      case AstLong(longValue)  => DeserializationSuccess(TypeTagged(BigInt(longValue), BigIntType))
      case AstInt(scalaBigInt) => DeserializationSuccess(TypeTagged(scalaBigInt, BigIntType))

      case AstDecimal(scalaBigDecimal) =>
        DeserializationResult(path)(TypeTagged(BigInt(scalaBigDecimal.bigDecimal.toBigIntegerExact), BigIntType), {
          case e: ArithmeticException =>
            DeserializationError.Malformed(e, reportedBy = self)
        })

      case AstDouble(doubleValue) =>
        DeserializationResult(path)(TypeTagged(BigInt(new java.math.BigDecimal(doubleValue).toBigIntegerExact), BigIntType), {
          case e: ArithmeticException =>
            DeserializationError.Malformed(e, reportedBy = self)
        })

      case AstString(s) if (guidance.isMapKey) => this.deserialize(path, ops.parse(s.asInstanceOf[S]))(ops, guidance = guidance.copy(isMapKey = false))

      case _                                   => DeserializationFailure(path, DeserializationError.Unexpected("Expected a JSON number (integer value)", reportedBy = self))
    }

}
