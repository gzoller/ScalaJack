package co.blocke.scalajack
package typeadapter

class EnumerationValueDeserializer[E <: Enumeration](enumeration: E)(implicit tt: TypeTag[E#Value]) extends Deserializer[E#Value] {

  self =>

  private val enumerationName: String = enumeration.getClass.getName
  private val enumerationValueType: Type = tt.tpe

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[E#Value] = {
    ast match {
      case AstString(name) =>
        DeserializationResult(path)(TypeTagged(enumeration.withName(name), enumerationValueType), {
          case _: NoSuchElementException =>
            DeserializationError.Malformed(s"Enumeration $enumerationName does not contain a value named $name", reportedBy = self)
        })

      case AstLong(index) =>
        DeserializationResult(path)(TypeTagged(enumeration(index.intValue), enumerationValueType), {
          case _: NoSuchElementException =>
            DeserializationError.Malformed(s"Enumeration $enumerationName does not contain a value at index $index", reportedBy = self)
        })

      case AstNull() =>
        DeserializationSuccess(TypeTagged(null, enumerationValueType))

      case _ =>
        DeserializationFailure(path, DeserializationError.Unexpected("Expected a JSON string or int", reportedBy = self))
    }

  }

}
