package co.blocke.scalajack
package parser

trait Path[T] {

  def parse[IN](ps: ParserState[IN])(implicit p: Parser[IN]): T

}

case class BooleanPath() extends Path[Boolean] {
  def parse[IN](ps: ParserState[IN])(implicit p: Parser[IN]): Boolean = p.consumeBoolean(ps)
}

case class IntPath() extends Path[Int] {
  def parse[IN](ps: ParserState[IN])(implicit p: Parser[IN]): Int = p.consume[BigInt](ps).intValue()
}

//case class IllegalPath() extends Path {
//  protected[parser] def parserHello( parser: Parser): Unit = {}
//}

case class UnexpectedException(msg: String) extends Exception(msg)

