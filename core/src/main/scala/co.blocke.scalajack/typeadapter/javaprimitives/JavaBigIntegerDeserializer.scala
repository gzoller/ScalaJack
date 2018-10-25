package co.blocke.scalajack
package typeadapter
package javaprimitives

class JavaBigIntegerDeserializer(scalaBigIntDeserializer: Deserializer[scala.math.BigInt]) extends Deserializer[java.math.BigInteger] {

  private val JavaBigIntegerType: Type = typeOf[java.math.BigInteger]

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[java.math.BigInteger] =
    scalaBigIntDeserializer.deserialize(path, ast) map {
      case TypeTagged(null)        => TypeTagged(null, JavaBigIntegerType)
      case TypeTagged(scalaBigInt) => TypeTagged(scalaBigInt.bigInteger, JavaBigIntegerType)
    }

}
