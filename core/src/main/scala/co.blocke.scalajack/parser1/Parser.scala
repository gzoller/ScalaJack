package co.blocke.scalajack
package parser1

//import scala.collection.mutable.LinkedHashMap

trait ParserState {
  type IN
}

trait Parser

trait BooleanParser extends Parser {
  def consumeBoolean(ps: ParserState): Boolean = throw new UnexpectedException(TypeAdapter.Root, "Expected a Boolean value")
}
trait IntParser extends Parser {
  def consumeInt(ps: ParserState): BigInt = throw new UnexpectedException(TypeAdapter.Root, "Expected an Integer value")
}

//trait KVParser[K, V, M <: Map[K, V], IN] extends Parser {
//  //  protected def consume(ps: ParserState[IN]) = consumeMap(ps)
//  val keyPath: Path[K]
//  val valuePath: Path[V]
//}

trait ArrayParser[E] extends Parser {
  def consumeArray(ps: ParserState): List[E] =
    throw new UnexpectedException(TypeAdapter.Root, "Expected an Array value")
  val elementTypeAdapter: TypeAdapter[E]
}