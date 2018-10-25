package co.blocke.scalajack

class DeferredSerializerReference[T](resolve: () => Serializer[T]) extends Serializer[T] {

  private lazy val resolved: Serializer[T] = resolve()

  override def serialize[AST, S](tagged: TypeTagged[T])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    resolved.serialize[AST, S](tagged)

}
