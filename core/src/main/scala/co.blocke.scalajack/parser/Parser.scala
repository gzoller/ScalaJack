package co.blocke.scalajack
package parser

import scala.collection.mutable.LinkedHashMap

trait ParserState[IN]

trait Parser[IN] {

  def consume[U](ps: ParserState[IN]): U

  def consumeBoolean(ps: ParserState[IN]): Boolean =
    throw new UnexpectedException("Expected a Boolean value")
  def consumeDecimal(ps: ParserState[IN]): BigDecimal =
    throw new UnexpectedException("Expected a Decimal value")
  def consumeInt(ps: ParserState[IN]): BigInt =
    throw new UnexpectedException("Expected an Integer value")
  def consumeString(ps: ParserState[IN]): String =
    throw new UnexpectedException("Expected a String value")
  def consumeMap[K, V](ps: ParserState[IN]): Map[K, V] =
    throw new UnexpectedException("Expected a Map value")
  def consumeArray[E](ps: ParserState[IN]): Array[E] =
    throw new UnexpectedException("Expected an Array value")
  def consumeObject[V](ps: ParserState[IN]): LinkedHashMap[String, V] =
    throw new UnexpectedException("Expected an Object value")
}

trait SimpleParser[IN] extends Parser[IN]

trait KVParser[K, V, M <: Map[K, V], IN] extends Parser[IN] {
  protected def consume(ps: ParserState[IN]) = consumeMap(ps)
  val keyPath: Path[K]
  val valuePath: Path[V]
}

trait ArrayParser[E, A <: Array[E], IN] extends Parser[IN] {
  def consume(ps: ParserState[IN]) = consumeArray(ps)
  val elementPath: Path[E]
}