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

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[Type] =
    json match {
      case JsonNull()           => DeserializationSuccess(nullTypeTagged)
      case JsonString(typeName) => DeserializationSuccess(TypeTagged(typeNameToType(typeName), TypeType))
    }

}
