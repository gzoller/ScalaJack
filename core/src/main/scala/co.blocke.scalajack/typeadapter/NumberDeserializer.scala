package co.blocke.scalajack
package typeadapter

/* This is basically like the java primitive equivalent, except this marshals Scala primitives */
case class NumberDeserializer() extends Deserializer[java.lang.Number] {

  self =>

  private val BoxedNumberType: Type = typeOf[java.lang.Number]
  private val BoxedDoubleType: Type = typeOf[Double]
  private val BoxedLongType: Type = typeOf[Long]
  private val ScalaBigDecimalType: Type = typeOf[scala.math.BigDecimal]
  private val ScalaBigIntegerType: Type = typeOf[scala.math.BigInt]

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[java.lang.Number] =
    ast match {
      case AstNull() => DeserializationSuccess(TypeTagged(null, BoxedNumberType))
      case AstDecimal(scalaBigDecimal) if (!scalaBigDecimal.isDecimalDouble) => DeserializationSuccess(TypeTagged(scalaBigDecimal, ScalaBigDecimalType))
      case AstDecimal(scalaBigDecimal) => DeserializationSuccess(TypeTagged(scalaBigDecimal.doubleValue(), BoxedDoubleType))
      case AstDouble(doubleValue) => DeserializationSuccess(TypeTagged(java.lang.Double.valueOf(doubleValue), BoxedDoubleType))
      case AstInt(scalaBigInt) => DeserializationSuccess(TypeTagged(scalaBigInt, ScalaBigIntegerType))
      case AstLong(longValue) => DeserializationSuccess(TypeTagged(java.lang.Long.valueOf(longValue), BoxedLongType))
      case AstString(s) if (guidance.isMapKey) => this.deserialize(path, ops.parse(s.asInstanceOf[S]))(ops, guidance = guidance.copy(isMapKey = false))
      case _ => DeserializationFailure(path, DeserializationError.Unsupported("Expected a Ast number", reportedBy = self))
    }
}
