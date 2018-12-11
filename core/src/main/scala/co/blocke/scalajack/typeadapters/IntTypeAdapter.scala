package co.blocke.scalajack
package typeadapters

import model._

object IntTypeAdapterFactory extends TypeAdapterFactory.=:=[Int] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[Int]): TypeAdapter[Int] =
    new IntTypeAdapter(context.flavor.getIntParser())

}

case class IntTypeAdapter(parser: Parser) extends TypeAdapter[Int] {

  override def materialize[AST](ast: AST)(implicit ops: Ops[AST]) = ast match {
    case AstInt(i) => i.intValue()
    case _         => throw new Exception("Boom Int")
  }

  override def dematerialize[AST](t: Int)(implicit ops: Ops[AST]): AST = AstInt(BigInt(t))
}
