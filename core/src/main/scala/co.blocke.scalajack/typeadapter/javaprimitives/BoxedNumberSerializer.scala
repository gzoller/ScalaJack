package co.blocke.scalajack
package typeadapter
package javaprimitives

class BoxedNumberSerializer() extends Serializer[java.lang.Number] {

  // Bizzare set of magic to try to "fix" the precision slop when moving from Float->Double (prints extra digits in JSON)
  private def capFloat(f: Float): Double = {
    val d = f.toString.toDouble
    val diff = f.toDouble - d
    f - diff
  }

  override def serialize[AST, S](tagged: TypeTagged[java.lang.Number])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(null)                                 => SerializationSuccess(AstNull())
      case TypeTagged(boxedByte: java.lang.Byte)            => SerializationSuccess(AstLong(boxedByte.longValue))
      case TypeTagged(boxedDouble: java.lang.Double)        => SerializationSuccess(AstDouble(boxedDouble.doubleValue))
      case TypeTagged(boxedFloat: java.lang.Float)          => SerializationSuccess(AstDouble(capFloat(boxedFloat.floatValue)))
      case TypeTagged(boxedInt: java.lang.Integer)          => SerializationSuccess(AstLong(boxedInt.longValue))
      case TypeTagged(boxedLong: java.lang.Long)            => SerializationSuccess(AstLong(boxedLong.longValue))
      case TypeTagged(boxedShort: java.lang.Short)          => SerializationSuccess(AstLong(boxedShort.longValue))
      case TypeTagged(javaBigInteger: java.math.BigInteger) => SerializationSuccess(AstInt(scala.math.BigInt(javaBigInteger)))
      case TypeTagged(javaBigDecimal: java.math.BigDecimal) => SerializationSuccess(AstDecimal(scala.math.BigDecimal(javaBigDecimal)))
    }

}
