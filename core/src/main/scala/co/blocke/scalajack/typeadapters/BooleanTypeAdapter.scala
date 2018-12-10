package co.blocke.scalajack
package typeadapters

import model._

object BooleanTypeAdapterFactory extends TypeAdapterFactory.=:=[Boolean] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[Boolean]): TypeAdapter[Boolean] =
    new BooleanTypeAdapter(context.flavor.getIntParser())

}

case class BooleanTypeAdapter(parser: Parser) extends TypeAdapter[Boolean] {

  override def materialize(primitive: AST_PRIMITIVE): Boolean = primitive match {
    case b: Boolean => b
    case _          => throw new Exception("Boom Boolean")
  }

  override def dematerialize(t: Boolean): AST_PRIMITIVE = t
}
