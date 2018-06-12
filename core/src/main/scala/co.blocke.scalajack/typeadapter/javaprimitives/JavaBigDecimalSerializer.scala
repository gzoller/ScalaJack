package co.blocke.scalajack
package typeadapter
package javaprimitives

object JavaBigDecimalSerializer {

  private val ScalaBigDecimalType: Type = typeOf[scala.math.BigDecimal]

}

class JavaBigDecimalSerializer(scalaBigDecimalSerializer: Serializer[scala.math.BigDecimal]) extends Serializer[java.math.BigDecimal] {

  import JavaBigDecimalSerializer.ScalaBigDecimalType

  override def serialize[J](tagged: TypeTagged[java.math.BigDecimal])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(null)           => scalaBigDecimalSerializer.serialize(TypeTagged(null, ScalaBigDecimalType))
      case TypeTagged(javaBigDecimal) => scalaBigDecimalSerializer.serialize(TypeTagged(scala.math.BigDecimal(javaBigDecimal), ScalaBigDecimalType))
    }

}
