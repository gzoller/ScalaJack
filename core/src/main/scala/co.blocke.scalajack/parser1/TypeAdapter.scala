package co.blocke.scalajack
package parser1

trait TypeAdapter[T] {
  val name: String = "*"
  def parse(ps: ParserState): T
  val parser: Parser // a parser capable of parsing this TypeAdapter
}

object TypeAdapter {
  val Root = new TypeAdapter[Nothing] {
    def parse(ps: ParserState) = ???
    val parser: Parser = null
  }
}

case class BooleanTypeAdapter(parser: BooleanParser) extends TypeAdapter[Boolean] {
  def parse(ps: ParserState): Boolean = parser.consumeBoolean(ps)
}

case class IntTypeAdapter(parser: IntParser) extends TypeAdapter[Int] {
  def parse(ps: ParserState): Int = parser.consumeInt(ps).intValue()
}

case class ListIntTypeAdapter(parser: ArrayParser[Int]) extends TypeAdapter[List[Int]] {
  def parse(ps: ParserState): List[Int] = parser.consumeArray(ps)
}
case class ListListIntTypeAdapter(parser: ArrayParser[List[Int]]) extends TypeAdapter[List[List[Int]]] {
  def parse(ps: ParserState): List[List[Int]] = parser.consumeArray(ps)
}

case class UnexpectedException(path: TypeAdapter[_], msg: String, index: Int = -1) extends Exception(msg)

// Amazing macro for dynamic "with" mixin of traits
// https://gist.github.com/xeno-by/2559714

