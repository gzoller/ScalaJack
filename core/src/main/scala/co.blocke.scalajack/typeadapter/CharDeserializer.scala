package co.blocke.scalajack
package typeadapter

class CharDeserializer extends Deserializer[Char] {

  self =>

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[Char] =
    ast match {
      case AstString(string) if string.length == 1 => DeserializationSuccess(TypeTagged(string.charAt(0)))
      case AstString(string)                       => DeserializationFailure(path, DeserializationError.Malformed(s"Expected a char (JSON string of length 1), not $string", reportedBy = self))
      case _                                       => DeserializationFailure(path, DeserializationError.Unexpected("Expected a char (JSON string of length 1)", reportedBy = self))
    }

}
