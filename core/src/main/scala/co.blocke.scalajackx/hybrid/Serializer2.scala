package co.blocke.scalajackx
package hybrid

trait ParserState {
  type WIRE
}

trait Parser {
  type AST
  type T

  def parse(ps: ParserState): AST // AST implementation
  def toPrimitives(ast: AST): Any // Part of AST implementation, e.g. Json4s
}

trait ArrayParser[E] extends Parser {
  val elementTypeAdapter: TypeAdapter[E]
}

trait TypeAdapter[T] {
  val parser: Parser
  def materialize(primitive: Any): T // Part of TypeAdapters
}

case class IntTypeAdapter(parser: Parser) extends TypeAdapter[Int] {
  override def materialize(primitive: Any): Int = primitive match {
    case b: BigInt => b.intValue()
    case _         => throw new Exception("Boom Int")
  }
}

case class ListTypeAdapter[T](parser: ArrayParser[T]) extends TypeAdapter[List[T]] {
  override def materialize(primitive: Any): List[T] = primitive match {
    case k: List[_] => k.map(e => parser.elementTypeAdapter.materialize(e))
    case _          => throw new Exception("Boom Array")
  }
}

class UnexpectedException(ta: TypeAdapter[_], msg: String, index: Int = -1) extends Exception(msg)