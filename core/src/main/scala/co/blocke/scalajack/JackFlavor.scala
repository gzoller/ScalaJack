package co.blocke.scalajack

import model._

trait JackFlavor[N] {
  this: ScalaJackLike[N] =>

  type AST
  type WIRE
  type PARSER_STATE

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
    val prim = toPrimitives(typeAdapaterOfT.parser.parse(genParserState(wire)))
    typeAdapaterOfT.materialize(prim)
  }

  def fastRead(wire: WIRE): N = {
    val prim = toPrimitives(nativeTypeAdapter.parser.parse(genParserState(wire)))
    nativeTypeAdapter.materialize(prim)
  }

  // Back 'n forth between AST and WIRE
  //------------------------------------
  def parse(src: WIRE): AST = {
    ???
    // PROBLEM:  How can I parse w/o a class or type T?
  }

  def emit(ast: AST): WIRE = genEmitterState().emit(toPrimitives(ast), false).result

  // Back 'n forth between Scala and AST
  //------------------------------------
  def materialize[T](ast: AST)(implicit tt: TypeTag[T]): T = {
    val typeAdapaterOfT = context.typeAdapterOf[T]
    typeAdapaterOfT.materialize(toPrimitives(ast))
  }

  def dematerialize[T](t: T)(implicit tt: TypeTag[T]): AST = {
    val typeAdapaterOfT = context.typeAdapterOf[T]
    fromPrimitives(typeAdapaterOfT.dematerialize(t))
  }
}
