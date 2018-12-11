package co.blocke.scalajack
package typeadapters

import model._

object BooleanTypeAdapterFactory extends TypeAdapter.=:=[Boolean] {

  override def materialize[AST](ast: AST)(implicit ops: Ops[AST]) = ast match {
    case AstBoolean(b) => b
    case _             => throw new Exception("Boom Boolean")
  }

  override def dematerialize[AST](t: Boolean)(implicit ops: Ops[AST]): AST = AstBoolean(t)
}
