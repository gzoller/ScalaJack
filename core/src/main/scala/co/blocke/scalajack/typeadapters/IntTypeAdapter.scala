package co.blocke.scalajack
package typeadapters

import model._

object IntTypeAdapterFactory extends TypeAdapter.=:=[Int] {

  override def materialize[AST](ast: AST)(implicit ops: Ops[AST]) = ast match {
    case AstInt(i) => i.intValue()
    case _         => throw new Exception("Boom Int")
  }

  override def dematerialize[AST](t: Int)(implicit ops: Ops[AST]): AST = AstInt(BigInt(t))
}
