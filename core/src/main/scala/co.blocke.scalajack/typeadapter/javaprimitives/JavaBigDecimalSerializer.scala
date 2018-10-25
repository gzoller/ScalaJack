package co.blocke.scalajack
package typeadapter
package javaprimitives

class JavaBigDecimalSerializer(scalaBigDecimalSerializer: Serializer[scala.math.BigDecimal]) extends Serializer[java.math.BigDecimal] {

  private val ScalaBigDecimalType: Type = typeOf[scala.math.BigDecimal]

  override def serialize[AST, S](tagged: TypeTagged[java.math.BigDecimal])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(null)           => scalaBigDecimalSerializer.serialize(TypeTagged(null, ScalaBigDecimalType))
      case TypeTagged(javaBigDecimal) => scalaBigDecimalSerializer.serialize(TypeTagged(scala.math.BigDecimal(javaBigDecimal), ScalaBigDecimalType))
    }

}
