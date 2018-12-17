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
//    String      (JString)

trait Parser { // This is the AST adapter
  type AST

  def parse(ps: ParserState): AST
  def toPrimitives(ast: AST): Any
  def fromPrimitives(prim: Any): AST
}

trait Emitter { // Wire writer
  type WIRE
  type EmitterState

  def emit(prim: Any, es: EmitterState): WIRE
}

trait Serializer2 extends Parser with Emitter
trait ArraySerializer[E] extends Serializer2 with ArrayParser[E] with Emitter

trait ArrayParser[E] extends Parser {
  val elementTypeAdapter: TypeAdapter[E]
}

trait TypeAdapter[T] {
  val serializer: Serializer2
  def materialize(primitive: Any): T
  def dematerialize(t: T): Any
}

case class IntTypeAdapter(serializer: Serializer2) extends TypeAdapter[Int] {
  override def materialize(primitive: Any): Int = primitive match {
    case b: BigInt => b.intValue()
    case _         => throw new Exception("Boom Int")
  }
  override def dematerialize(t: Int): Any = BigInt(t)
}

case class ListTypeAdapter[T](serializer: ArraySerializer[T]) extends TypeAdapter[List[T]] {
  override def materialize(primitive: Any): List[T] = primitive match {
    case k: List[_] => k.map(e => serializer.elementTypeAdapter.materialize(e))
    case _          => throw new Exception("Boom Array")
  }
  override def dematerialize(t: List[T]): Any = t.map(e => serializer.elementTypeAdapter.dematerialize(e))
}

class UnexpectedException(ta: TypeAdapter[_], msg: String, index: Int = -1) extends Exception(msg)