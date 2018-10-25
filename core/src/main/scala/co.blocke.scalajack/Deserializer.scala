package co.blocke.scalajack

object Deserializer {

  def constant[T](tagged: TypeTagged[T]): Deserializer[T] =
    new Deserializer[T] {

      self =>

      override def deserializeFromNothing[AST, S](path: Path)(implicit ops: AstOps[AST, S]): DeserializationResult[T] =
        DeserializationSuccess(tagged)

      override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[T] =
        DeserializationFailure(path, DeserializationError.Unsupported(s"Expected no JSON at path $path because value is constant", reportedBy = self))

    }

}

trait Deserializer[+T] {

  self =>

  def deserializeFromNothing[AST, S](path: Path)(implicit ops: AstOps[AST, S]): DeserializationResult[T] =
    DeserializationFailure(path, DeserializationError.Missing(reportedBy = self))

  def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[T]

}
