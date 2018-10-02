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

  override def serialize[J](tagged: TypeTagged[java.lang.Number])(implicit ops: JsonOps[J], guidance: SerializationGuidance): SerializationResult[J] =
    tagged match {
      case TypeTagged(null)                                 => SerializationSuccess(JsonNull())
      case TypeTagged(boxedByte: java.lang.Byte)            => SerializationSuccess(JsonLong(boxedByte.longValue))
      case TypeTagged(boxedDouble: java.lang.Double)        => SerializationSuccess(JsonDouble(boxedDouble.doubleValue))
      case TypeTagged(boxedFloat: java.lang.Float)          => SerializationSuccess(JsonDouble(capFloat(boxedFloat.floatValue)))
      case TypeTagged(boxedInt: java.lang.Integer)          => SerializationSuccess(JsonLong(boxedInt.longValue))
      case TypeTagged(boxedLong: java.lang.Long)            => SerializationSuccess(JsonLong(boxedLong.longValue))
      case TypeTagged(boxedShort: java.lang.Short)          => SerializationSuccess(JsonLong(boxedShort.longValue))
      case TypeTagged(javaBigInteger: java.math.BigInteger) => SerializationSuccess(JsonInt(scala.math.BigInt(javaBigInteger)))
      case TypeTagged(javaBigDecimal: java.math.BigDecimal) => SerializationSuccess(JsonDecimal(scala.math.BigDecimal(javaBigDecimal)))
    }

}
