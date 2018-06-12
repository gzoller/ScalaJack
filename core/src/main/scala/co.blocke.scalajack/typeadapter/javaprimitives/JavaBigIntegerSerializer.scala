package co.blocke.scalajack
package typeadapter
package javaprimitives

object JavaBigIntegerSerializer {

  private val ScalaBigIntType: Type = typeOf[scala.math.BigInt]

}

class JavaBigIntegerSerializer(scalaBigIntSerializer: Serializer[scala.math.BigInt]) extends Serializer[java.math.BigInteger] {

  import JavaBigIntegerSerializer.ScalaBigIntType

  override def serialize[J](tagged: TypeTagged[java.math.BigInteger])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(null)           => scalaBigIntSerializer.serialize(TypeTagged(null, ScalaBigIntType))
      case TypeTagged(javaBigInteger) => scalaBigIntSerializer.serialize(TypeTagged(scala.math.BigInt(javaBigInteger), ScalaBigIntType))
    }

}
