package co.blocke.scalajack
package typeadapter
package javaprimitives

class BoxedNumberDeserializer() extends Deserializer[java.lang.Number] {

  self =>

  private val BoxedNumberType: Type = typeOf[java.lang.Number]
  private val BoxedDoubleType: Type = typeOf[java.lang.Double]
  private val BoxedLongType: Type = typeOf[java.lang.Long]
  private val JavaBigDecimalType: Type = typeOf[java.math.BigDecimal]
  private val JavaBigIntegerType: Type = typeOf[java.math.BigInteger]

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[java.lang.Number] =
    ast match {
      case AstNull() => DeserializationSuccess(TypeTagged(null, BoxedNumberType))
      case AstDecimal(scalaBigDecimal) if (!scalaBigDecimal.isDecimalDouble) => DeserializationSuccess(TypeTagged(scalaBigDecimal.bigDecimal, JavaBigDecimalType))
      case AstDecimal(scalaBigDecimal) => DeserializationSuccess(TypeTagged(scalaBigDecimal.doubleValue, BoxedDoubleType))
      case AstDouble(doubleValue) => DeserializationSuccess(TypeTagged(java.lang.Double.valueOf(doubleValue), BoxedDoubleType))
      case AstInt(scalaBigInt) => DeserializationSuccess(TypeTagged(scalaBigInt.bigInteger, JavaBigIntegerType))
      case AstLong(longValue) => DeserializationSuccess(TypeTagged(java.lang.Long.valueOf(longValue), BoxedLongType))
      case AstString(s) if (guidance.isMapKey) => this.deserialize(path, ops.parse(s.asInstanceOf[S]))(ops, guidance = guidance.copy(isMapKey = false))
      case AstString(_) => DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON number", reportedBy = self))
      case _ => DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON number", reportedBy = self))
    }
}
