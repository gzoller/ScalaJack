package co.blocke.scalajack

class TransformedSerializer[T](wrappedSerializer: Serializer[T]) extends Serializer[T] {

  def serialize[AST, S](tagged: TypeTagged[T])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] = {
    wrappedSerializer.serialize(tagged)
  }

}
