package co.blocke.scalajack

trait TypeAdapter[T] {
  val serializer: Serializer
}

object TypeAdapter {
  val Root = new TypeAdapter[Nothing] {
    val parser: Parser = null
  }
}

case class BooleanTypeAdapter(parser: Parser) extends TypeAdapter[Boolean]

case class IntTypeAdapter(parser: Parser) extends TypeAdapter[Int]
//{
//  def parse[AST](ps: ParserState): AST = parser.parse(ps)
//}

case class ListIntTypeAdapter(parser: ArrayParser[Int]) extends TypeAdapter[List[Int]]
//{
//  def parse[AST](ps: ParserState): AST = parser.parse(ps)
//}

case class ListListIntTypeAdapter(parser: ArrayParser[List[Int]]) extends TypeAdapter[List[List[Int]]]
//{
//  def parse[AST](ps: ParserState): AST = parser.parse(ps)
//}

case class UnexpectedException(path: TypeAdapter[_], msg: String, index: Int = -1) extends Exception(msg)

// Amazing macro for dynamic "with" mixin of traits
// https://gist.github.com/xeno-by/2559714

