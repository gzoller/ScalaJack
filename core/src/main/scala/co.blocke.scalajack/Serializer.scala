package co.blocke.scalajack

trait Serializer[T] {

  def serialize[AST, S](tagged: TypeTagged[T])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST]

}
