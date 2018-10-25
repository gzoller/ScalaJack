package co.blocke.scalajack

class TermSerializer[T](next: Serializer[T]) extends Serializer[T] {

  override def serialize[AST, S](tagged: TypeTagged[T])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    next.serialize(tagged) // TODO implement this method

}
