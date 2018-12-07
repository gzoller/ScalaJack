package co.blocke.scalajackx
package uroll

trait ParserState {
  type WIRE
}

trait Parser {
  type AST
  type T

  def parse(ps: ParserState): AST
  def materialize(ast: AST): T
}

trait ArrayParser[E] extends Parser {
  val elementTypeAdapter: TypeAdapter[E]
}

trait TypeAdapter[T] {
  val parser: Parser
}

case class IntTypeAdapter(parser: Parser) extends TypeAdapter[Int]
case class ListTypeAdapter[T](parser: ArrayParser[T]) extends TypeAdapter[List[T]]

//
//case class ListIntTypeAdapter(parser: ArrayParser[Int]) extends TypeAdapter[List[Int]]
//case class ListListIntTypeAdapter(parser: ArrayParser[List[Int]]) extends TypeAdapter[List[List[Int]]]

class UnexpectedException(ta: TypeAdapter[_], msg: String, index: Int = -1) extends Exception(msg)