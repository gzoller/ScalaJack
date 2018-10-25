package co.blocke.scalajack
package typeadapter

class StringDeserializer extends Deserializer[String] {

  self =>

  private val StringType: Type = typeOf[String]

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[String] =
    ast match {
      case AstNull()        => DeserializationSuccess(TypeTagged(null, StringType))
      case AstString(value) => DeserializationSuccess(TypeTagged(value, StringType))
      case _                => DeserializationFailure(path, DeserializationError.Unexpected("Expected a JSON string", reportedBy = self))
    }

}
