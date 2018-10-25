package co.blocke.scalajack

trait JackFlavor[AST, S] {
  implicit val guidance: SerializationGuidance

  def render[T](instance: T)(implicit tt: TypeTag[T]): S

  // No exceptions on failure -- Left return on Either for failures
  def readSafely[T](src: S)(implicit tt: TypeTag[T]): Either[DeserializationFailure, T]

  def read[T](src: S)(implicit tt: TypeTag[T]): T =
    readSafely[T](src) match {
      case Right(x) => x
      case Left(x)  => throw new DeserializationException(x)
    }

  def parse(src: S): AST
  def emit(ast: AST): S

  // TODO
  //  def become[N](ast: AST)(implicit becomeFn: (AST) => N): N = becomeFn(ast)
}
