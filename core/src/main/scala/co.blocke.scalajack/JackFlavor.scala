package co.blocke.scalajack

trait JackFlavor[S, AST] {
  implicit val guidance: SerializationGuidance

  def read[T](src: S)(implicit tt: TypeTag[T]): T
  def render[T](instance: T)(implicit tt: TypeTag[T]): S

  def parse(src: S): AST
  def render(ast: AST): S

  def become[N](ast: AST)(implicit becomeFn: (AST) => N): N = becomeFn(ast)
}
