package co.blocke.scalajack
package typeadapter

class FloatDeserializer extends Deserializer[Float] {

  self =>

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[Float] =
    ast match {
      case AstDecimal(bigDecimal) if bigDecimal.isDecimalFloat =>
        DeserializationResult(path)(TypeTagged(bigDecimal.toFloat), {
          case e: ArithmeticException =>
            DeserializationError.Malformed(e, reportedBy = self)
        })
      case AstDecimal(_) => DeserializationFailure(path, DeserializationError.Unexpected("XFloat value out of range", reportedBy = self))

      case AstDouble(doubleValue) =>
        DeserializationResult(path)(TypeTagged(doubleValue.toFloat), {
          case e: ArithmeticException =>
            DeserializationError.Malformed(e, reportedBy = self)
        })

      case AstLong(longValue) if (longValue >= java.lang.Float.MIN_VALUE && longValue <= java.lang.Float.MAX_VALUE) =>
        DeserializationResult(path)(TypeTagged(longValue.toFloat), {
          case e: ArithmeticException =>
            DeserializationError.Malformed(e, reportedBy = self)
        })
      case AstLong(_) => DeserializationFailure(path, DeserializationError.Unexpected("Float value out of range", reportedBy = self))

      case AstInt(intValue) =>
        DeserializationResult(path)(TypeTagged(intValue.toFloat), {
          case e: ArithmeticException =>
            DeserializationError.Malformed(e, reportedBy = self)
        })

      case AstString(s) if (guidance.isMapKey) => this.deserialize(path, ops.parse(s.asInstanceOf[S]))(ops, guidance = guidance.copy(isMapKey = false))

      case _ =>
        DeserializationFailure(path, DeserializationError.Unexpected("Expected a JSON number", reportedBy = self))
    }

}
