package co.blocke.scalajackx

trait ParserState

trait AstAdapter {
  type AST

  val ops: AstOps[AST]
}

trait WireSerializer {
  this: AstAdapter =>

  type WIRE

  protected def createParserState(wire: WIRE): ParserState

  final def parse(wire: WIRE): AST = _parse(createParserState(wire))
  def _parse(ps: ParserState): AST
  def emit(ast: AST): WIRE
}

trait Serializer extends AstAdapter with WireSerializer

/*
// E must be an AST Primitive
trait ArraySerializer[E] extends Serializer {
  type PRIMITIVE = List[E]
  val elementTypeAdapter: TypeAdapter[E]
}

trait BooleanSerializer extends Serializer {
  type PRIMITIVE = Boolean
}

trait DecimalSerializer extends Serializer {
  type PRIMITIVE = BigDecimal
}

trait IntSerializer extends Serializer {
  type PRIMITIVE = BigInt
}

// V must be an AST Primitive
trait ObjectSerializer[V] extends Serializer {
  type PRIMITIVE = scala.collection.mutable.LinkedHashMap[String, V]
  val valueTypeAdapter: TypeAdapter[V]
}

// K and V must be AST Primitives
trait MapSerializer[K, V] extends Serializer {
  type PRIMITIVE = Map[K, V]
  val keyTypeAdapter: TypeAdapter[K]
  val valueTypeAdapter: TypeAdapter[V]
}

trait StringSerializer extends Serializer {
  type PRIMITIVE = String
}
*/

