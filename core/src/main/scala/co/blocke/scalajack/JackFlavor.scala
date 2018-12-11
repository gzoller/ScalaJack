package co.blocke.scalajack

import model._

trait JackFlavor[N] {
  this: ScalaJackLike[N] =>

  type AST
  type WIRE
  type PARSER_STATE

  implicit val ops: Ops[AST]

  protected def genEmitterState(): EmitterState[WIRE]
  protected def genParserState(input: WIRE): PARSER_STATE

  // Primitives
  def getArrayParser[E]()(implicit tt: TypeTag[E]): ArrayParser[E]
  def getBooleanParser(): Parser
  def getIntParser(): Parser

  protected def toPrimitives(ast: AST): AST_PRIMITIVE
  protected def fromPrimitives(prim: AST_PRIMITIVE): AST

  /*
  implicit val guidance: SerializationGuidance
  implicit val ops: Ops[IR, WIRE]
  */

  def render[T](instance: T)(implicit tt: TypeTag[T]): WIRE = {
    val typeAdapaterOfT = context.typeAdapterOf[T]
    genEmitterState().emit(typeAdapaterOfT.dematerialize(instance), false).result
  }

  /*
  // No exceptions on failure -- Left return on Either for failures
  def readSafely[T](wire: WIRE)(implicit tt: TypeTag[T]): Either[ReadFailure, T] = {
    val irTransceiver = context.typeAdapterOf[T].irTransceiver
    ops.deserialize(Path.Root, wire).mapToReadResult(Path.Root, (dsIR: IR) => irTransceiver.read(Path.Root, dsIR)) match {
      case rs: ReadSuccess[T] => Right(rs.get)
      case rf: ReadFailure    => Left(rf)
    }
  }
  */

  def read[T](wire: WIRE)(implicit tt: TypeTag[T]): T = {
    val typeAdapaterOfT = context.typeAdapterOf[T]
    typeAdapaterOfT.materialize(typeAdapaterOfT.parser.parse(genParserState(wire)))
  }

  def fastRead(wire: WIRE): N =
    nativeTypeAdapter.materialize(nativeTypeAdapter.parser.parse(genParserState(wire)))

  // Back 'n forth between AST and WIRE
  //------------------------------------
  def parse(src: WIRE): AST = {
    ???
    // PROBLEM:  How can I parse w/o a class or type T?
  }

  def emit(ast: AST): WIRE = genEmitterState().emit(toPrimitives(ast), false).result

  // Back 'n forth between Scala and AST
  //------------------------------------
  def materialize[T](ast: AST)(implicit tt: TypeTag[T], ops: Ops[AST]): T =
    context.typeAdapterOf[T].materialize(ast)

  def dematerialize[T](t: T)(implicit tt: TypeTag[T], ops: Ops[AST]): AST =
    context.typeAdapterOf[T].dematerialize(t)
}
