package co.blocke.scalajack

trait Renderer[S] {
  def renderCompact[AST](ast: AST, sj: ScalaJackLike[_, _])(implicit ops: AstOps[AST, S]): S
}
