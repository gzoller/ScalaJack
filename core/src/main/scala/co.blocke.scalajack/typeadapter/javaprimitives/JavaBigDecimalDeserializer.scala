package co.blocke.scalajack
package typeadapter
package javaprimitives

class JavaBigDecimalDeserializer(scalaBigDecimalDeserializer: Deserializer[scala.math.BigDecimal]) extends Deserializer[java.math.BigDecimal] {

  private val JavaBigDecimalType: Type = typeOf[java.math.BigDecimal]

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[java.math.BigDecimal] =
    scalaBigDecimalDeserializer.deserialize(path, ast) map {
      case TypeTagged(null)            => TypeTagged(null, JavaBigDecimalType)
      case TypeTagged(scalaBigDecimal) => TypeTagged(scalaBigDecimal.bigDecimal, JavaBigDecimalType)
    }

}
