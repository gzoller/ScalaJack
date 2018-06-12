package co.blocke.scalajack
package typeadapter
package javaprimitives

import co.blocke.scalajack.typeadapter.javaprimitives.JavaBigDecimalDeserializer.JavaBigDecimalType

import scala.reflect.runtime.universe.{ Type, typeOf }

object JavaBigDecimalDeserializer {

  val JavaBigDecimalType: Type = typeOf[java.math.BigDecimal]

}

class JavaBigDecimalDeserializer(scalaBigDecimalDeserializer: Deserializer[scala.math.BigDecimal]) extends Deserializer[java.math.BigDecimal] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[java.math.BigDecimal] =
    scalaBigDecimalDeserializer.deserialize(path, json) map {
      case TypeTagged(null)            => TypeTagged(null, JavaBigDecimalType)
      case TypeTagged(scalaBigDecimal) => TypeTagged(scalaBigDecimal.bigDecimal, JavaBigDecimalType)
    }

}
