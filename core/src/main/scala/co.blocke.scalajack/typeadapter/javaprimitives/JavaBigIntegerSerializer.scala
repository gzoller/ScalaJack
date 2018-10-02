package co.blocke.scalajack
package typeadapter
package javaprimitives

class JavaBigIntegerSerializer(scalaBigIntSerializer: Serializer[scala.math.BigInt]) extends Serializer[java.math.BigInteger] {

  private val ScalaBigIntType: Type = typeOf[scala.math.BigInt]

  override def serialize[J](tagged: TypeTagged[java.math.BigInteger])(implicit ops: JsonOps[J], guidance: SerializationGuidance): SerializationResult[J] =
    tagged match {
      case TypeTagged(null)           => scalaBigIntSerializer.serialize(TypeTagged(null, ScalaBigIntType))
      case TypeTagged(javaBigInteger) => scalaBigIntSerializer.serialize(TypeTagged(scala.math.BigInt(javaBigInteger), ScalaBigIntType))
    }

}
