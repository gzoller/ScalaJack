package co.blocke.scalajack
package typeadapter
package javaprimitives

class BoxedNumberSerializer() extends Serializer[java.lang.Number] {

  override def serialize[J](tagged: TypeTagged[java.lang.Number])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(null)                                 => SerializationSuccess(JsonNull())
      case TypeTagged(boxedByte: java.lang.Byte)            => SerializationSuccess(JsonLong(boxedByte.longValue))
      case TypeTagged(boxedDouble: java.lang.Double)        => SerializationSuccess(JsonDouble(boxedDouble.doubleValue))
      case TypeTagged(boxedFloat: java.lang.Float)          => SerializationSuccess(JsonDouble(boxedFloat.doubleValue))
      case TypeTagged(boxedInt: java.lang.Integer)          => SerializationSuccess(JsonLong(boxedInt.longValue))
      case TypeTagged(boxedLong: java.lang.Long)            => SerializationSuccess(JsonLong(boxedLong.longValue))
      case TypeTagged(boxedShort: java.lang.Short)          => SerializationSuccess(JsonLong(boxedShort.longValue))
      case TypeTagged(javaBigInteger: java.math.BigInteger) => SerializationSuccess(JsonInt(scala.math.BigInt(javaBigInteger)))
      case TypeTagged(javaBigDecimal: java.math.BigDecimal) => SerializationSuccess(JsonDecimal(scala.math.BigDecimal(javaBigDecimal)))
    }

}
