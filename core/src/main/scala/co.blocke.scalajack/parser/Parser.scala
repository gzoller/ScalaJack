package co.blocke.scalajack
package parser

trait ParserState

trait Parser {
//  protected[parser] def consume[T]( ps: ParserState ): T

  private[parser] def consumeBoolean( ps: ParserState ): Boolean =
    throw new UnexpectedException("Expected a Boolean value")
  private[parser] def consumeDecimal( ps: ParserState ): BigDecimal =
    throw new UnexpectedException("Expected a Decimal value")
  private[parser] def consumeInt( ps: ParserState ): BigInt =
    throw new UnexpectedException("Expected a Integer value")
  private[parser] def consumeString( ps: ParserState ): String =
    throw new UnexpectedException("Expected a String value")

  def init(path: Path): Unit = {}
}

trait BooleanParser extends Parser {
  protected[parser] def consume( ps: ParserState ) = consumeBoolean(ps)
}

trait DecimalParser extends Parser {
  protected[parser] def consume( ps: ParserState ) = consumeDecimal(ps)
}

trait IntParser extends Parser {
  protected[parser] def consume( ps: ParserState ) = consumeInt(ps)
}

trait StringParser extends Parser {
  protected[parser] def consume( ps: ParserState ) = consumeString(ps)
}

trait KVParser extends Parser {
  protected[parser] def consume[K,V,M <: Map[K,V]]( ps: ParserState ): M
  val keyPath: Path
  val valuePath: Path
}

trait ArrayParser extends Parser {
  protected[parser] def consume[E,A <: Array[E]]( ps: ParserState ): A
  val elementPath: Path
}