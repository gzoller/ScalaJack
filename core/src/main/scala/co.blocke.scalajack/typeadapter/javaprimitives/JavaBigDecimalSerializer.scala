package co.blocke.scalajack
package typeadapter
package javaprimitives

import co.blocke.scalajack.typeadapter.javaprimitives.JavaBigDecimalSerializer.ScalaBigDecimalType

import scala.reflect.runtime.universe.{ Type, typeOf }

object JavaBigDecimalSerializer {

  val ScalaBigDecimalType: Type = typeOf[scala.math.BigDecimal]

}

class JavaBigDecimalSerializer(scalaBigDecimalSerializer: Serializer[scala.math.BigDecimal]) extends Serializer[java.math.BigDecimal] {

  override def serialize[J](tagged: TypeTagged[java.math.BigDecimal])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(null)           => scalaBigDecimalSerializer.serialize(TypeTagged(null, ScalaBigDecimalType))
      case TypeTagged(javaBigDecimal) => scalaBigDecimalSerializer.serialize(TypeTagged(scala.math.BigDecimal(javaBigDecimal), ScalaBigDecimalType))
    }

}
