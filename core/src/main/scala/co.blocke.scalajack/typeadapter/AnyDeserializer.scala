package co.blocke.scalajack
package typeadapter

class AnyDeserializer(
    typeDeserializer:    Deserializer[Type],
    mapDeserializer:     Deserializer[Map[Any, Any]],
    listDeserializer:    Deserializer[List[Any]],
    stringDeserializer:  Deserializer[String],
    numberDeserializer:  Deserializer[java.lang.Number],
    booleanDeserializer: Deserializer[Boolean],
    context:             Context) extends Deserializer[Any] {

  self =>

  private val nullTypeTagged = TypeTagged(null, typeOf[Any])

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[Any] =
    ast match {
      // For map keys of type Any, all of these will be string.  We need to test and see if we can deserialize a specific type or not.
      // If not, stay with the string, otherwise use the more specific type.
      case AstString(s) if (guidance.isMapKey) =>
        try {
          context.typeAdapterOf[Any].deserializer.deserialize(path, ops.parse(s.asInstanceOf[S])) match {
            case success: DeserializationSuccess[_] => success
            case _                                  => stringDeserializer.deserialize(path, ast)
          }
        } catch {
          // Nope... no embedded typed thing found... must be a plain 'ol String
          case _: Throwable => stringDeserializer.deserialize(path, ast)
        }

      case AstObject(x) =>
        val fields = x.asInstanceOf[ops.ObjectFields]

        val concreteTypeFieldName = context.defaultHint

        ops.getObjectField(fields, concreteTypeFieldName) match {
          case Some(concreteTypeAst) =>
            val DeserializationSuccess(TypeTagged(concreteType)) = typeDeserializer.deserialize(path \ concreteTypeFieldName, concreteTypeAst)
            context.typeAdapter(concreteType).deserializer.deserialize(path, ast)

          case None =>
            mapDeserializer.deserialize(path, ast)
        }

      case AstArray(_) =>
        listDeserializer.deserialize(path, ast)

      case AstString(_) =>
        stringDeserializer.deserialize(path, ast)

      case AstBoolean(_) =>
        booleanDeserializer.deserialize(path, ast)

      case AstDouble(_) | AstDecimal(_) | AstInt(_) | AstLong(_) =>
        numberDeserializer.deserialize(path, ast)

      case AstNull() =>
        DeserializationSuccess(nullTypeTagged)

      case _ => DeserializationFailure(path, DeserializationError.Unexpected(s"Given value is of unknown type: $ast", reportedBy = self))
    }

}
