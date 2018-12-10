package co.blocke.scalajack
package typeadapters

import model._

object IntTypeAdapterFactory extends TypeAdapterFactory.=:=[Int] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[Int]): TypeAdapter[Int] =
    new IntTypeAdapter(context.flavor.getIntParser())

}

case class IntTypeAdapter(parser: Parser) extends TypeAdapter[Int] {

  override def materialize(primitive: AST_PRIMITIVE): Int = primitive match {
    case b: BigInt => b.intValue()
    case _         => throw new Exception("Boom Int")
  }

  override def dematerialize(t: Int): AST_PRIMITIVE = BigInt(t)
}
