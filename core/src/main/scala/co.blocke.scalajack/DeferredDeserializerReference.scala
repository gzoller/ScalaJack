package co.blocke.scalajack

class DeferredDeserializerReference[T](resolve: () => Deserializer[T]) extends Deserializer[T] {

  private lazy val resolved: Deserializer[T] = resolve()

  override def deserializeFromNothing[AST, S](path: Path)(implicit ops: AstOps[AST, S]): DeserializationResult[T] =
    resolved.deserializeFromNothing[AST, S](path)

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[T] =
    resolved.deserialize[AST, S](path, ast)

}
