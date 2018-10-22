package co.blocke.scalajack

trait JackFlavor[S, AST] {
  implicit val guidance: SerializationGuidance

  def read[T](src: S)(implicit tt: TypeTag[T]): T
  def render[T](instance: T)(implicit tt: TypeTag[T]): S

  // No exceptions on failure -- Left return on Either for failures
  def readSafely[T](src: S)(implicit tt: TypeTag[T]): Either[DeserializationFailure, T]

  def parse(src: S): AST
  def emit(ast: AST): S

  // TODO
  //  def become[N](ast: AST)(implicit becomeFn: (AST) => N): N = becomeFn(ast)
}
