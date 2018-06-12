package co.blocke.scalajack
package typeadapter
package javaprimitives

import co.blocke.scalajack.typeadapter.javaprimitives.JavaBigIntegerSerializer.ScalaBigIntType

import scala.reflect.runtime.universe.{ Type, typeOf }

object JavaBigIntegerSerializer {

  val ScalaBigIntType: Type = typeOf[scala.math.BigInt]

}

class JavaBigIntegerSerializer(scalaBigIntSerializer: Serializer[scala.math.BigInt]) extends Serializer[java.math.BigInteger] {

  override def serialize[J](tagged: TypeTagged[java.math.BigInteger])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(null)           => scalaBigIntSerializer.serialize(TypeTagged(null, ScalaBigIntType))
      case TypeTagged(javaBigInteger) => scalaBigIntSerializer.serialize(TypeTagged(scala.math.BigInt(javaBigInteger), ScalaBigIntType))
    }

}
