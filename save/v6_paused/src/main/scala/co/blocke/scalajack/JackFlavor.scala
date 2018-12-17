package co.blocke.scalajack

import model._
import util.Path

trait JackFlavor[N, AST, WIRE] {
  this: ScalaJackLike[N, AST, WIRE] =>

  implicit val ops: Ops[AST]
  implicit val g = SerializationGuidance(this)

  type WIRE_TYPE = WIRE // used in CaseClassTypeAdapter to "save" the parameterized type

  def render[T](instance: T)(implicit tt: TypeTag[T]): WIRE = emit(context.typeAdapterOf[T].write(instance))

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

  def read[T](wire: WIRE)(implicit tt: TypeTag[T]): T = context.typeAdapterOf[T].read(Path.Root, parse(wire))
  def fastRead(wire: WIRE): N = nativeTypeAdapter.read(Path.Root, parse(wire))

  // Back 'n forth between AST and WIRE
  //------------------------------------
  def parse(src: WIRE): AST
  def emit(ast: AST): WIRE

  // Back 'n forth between Scala and AST
  //------------------------------------
  def materialize[T](path: Path, ast: AST)(implicit tt: TypeTag[T], ops: Ops[AST]): T = context.typeAdapterOf[T].read(path, ast)
  def dematerialize[T](t: T)(implicit tt: TypeTag[T], ops: Ops[AST]): AST = context.typeAdapterOf[T].write(t)
}
