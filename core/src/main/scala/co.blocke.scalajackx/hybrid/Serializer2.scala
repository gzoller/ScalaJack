package co.blocke.scalajackx
package hybrid

trait ParserState {
  type WIRE
}

// AST Primitives (from Json4s but applies universally)
//    BigInt      (JInt)
//    BigDecimal  (JDecimal)
//    Boolean     (JBool)
//    List[<primitive>]  (JArray)
//    Map[String, <primitive>]  (JObject)
//    null        (JNull)
//    String      (JString)0

trait Parser {
  type AST

  def parse(ps: ParserState): AST
  def toPrimitives(ast: AST): Any
  def fromPrimitives(prim: Any): AST
}

//trait Emitter {
//  type WIRE
//
//  def emit( es: EmitState ): WIRE
//}

trait ArrayParser[E] extends Parser {
  val elementTypeAdapter: TypeAdapter[E]
}

trait TypeAdapter[T] {
  val parser: Parser
  def materialize(primitive: Any): T
  def dematerialize(t: T): Any
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