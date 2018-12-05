package co.blocke.scalajack
package parser2

trait Parser {
  type AST
  type T

  def parse(ps: ParserState): AST
  def materialize(ast: AST): T
}

//trait BooleanParser extends Parser {
//  def parse(ps: ParserState): AST = throw new UnexpectedException(TypeAdapter.Root, "Expected a Boolean value")
//}
//
//trait IntParser extends Parser {
//  def parse(ps: ParserState): AST = throw new UnexpectedException(TypeAdapter.Root, "Expected an Integer value")
//}

trait ArrayParser[E] extends Parser {
  val elementTypeAdapter: TypeAdapter[E]
  def parse(ps: ParserState): AST = throw new UnexpectedException(TypeAdapter.Root, "Expected an Array value")
}
