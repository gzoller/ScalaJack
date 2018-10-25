package co.blocke.scalajack

trait Parser[S] {
  def parse[AST](source: S)(implicit ops: AstOps[AST, S]): Option[AST]
}

