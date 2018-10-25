package co.blocke.scalajack
package typeadapter
package javaprimitives

class BoxedByteDeserializer(byteDeserializer: Deserializer[Byte]) extends Deserializer[java.lang.Byte] {

  private val BoxedByteType: Type = typeOf[java.lang.Byte]

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[java.lang.Byte] =
    ast match {
      case AstNull() =>
        DeserializationSuccess(TypeTagged(null, BoxedByteType))

      case _ =>
        byteDeserializer.deserialize(path, ast) map {
          case TypeTaggedByte(byteValue) => TypeTagged(java.lang.Byte.valueOf(byteValue), BoxedByteType)
          case TypeTagged(byteValue)     => TypeTagged(java.lang.Byte.valueOf(byteValue), BoxedByteType)
        }
    }

}
