package co.blocke.scalajackx

trait TypeAdapter[T] {
  val serializer: Serializer

  def unpackAST(ast: serializer.AST)(implicit astValues: ASTValues): T
  def packAST(t: T)(implicit astValues: ASTValues): serializer.AST
}

// Amazing macro for dynamic "with" mixin of traits
// https://gist.github.com/xeno-by/2559714

