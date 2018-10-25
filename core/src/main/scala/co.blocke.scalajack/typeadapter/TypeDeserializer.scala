package co.blocke.scalajack
package typeadapter

object TypeDeserializer {

  def typeNameToType(typeName: String): Type =
    try {
      staticClass(typeName).toType
    } catch {
      case e: ScalaReflectionException =>
        throw new ClassNotFoundException(s"""Unable to find class named "$typeName"\n""", e)
    }

}

class TypeDeserializer(typeNameToType: String => Type = TypeDeserializer.typeNameToType) extends Deserializer[Type] {

  private val TypeType: Type = typeOf[Type]
  private val nullTypeTagged = TypeTagged(null, TypeType)

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[Type] =
    ast match {
      case AstNull()           => DeserializationSuccess(nullTypeTagged)
      case AstString(typeName) => DeserializationSuccess(TypeTagged(typeNameToType(typeName), TypeType))
    }

}
