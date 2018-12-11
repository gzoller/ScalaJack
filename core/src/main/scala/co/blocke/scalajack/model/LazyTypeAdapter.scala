package co.blocke.scalajack
package model

case class LazyTypeAdapter[T](context: Context, tpe: Type) extends TypeAdapter[T] {

  var resolvedTypeAdapter: TypeAdapter[T] = _

  override def resolved: TypeAdapter[T] = {
    var typeAdapter = resolvedTypeAdapter

    // $COVERAGE-OFF$Can't really test as this is triggered by race condition, if it can happen at all.
    if (typeAdapter == null) {
      typeAdapter = context.typeAdapter(tpe).resolved.asInstanceOf[TypeAdapter[T]]
      if (typeAdapter.isInstanceOf[LazyTypeAdapter[_]]) {
        throw new IllegalStateException(s"Type adapter for $tpe is still being built")
      }
      resolvedTypeAdapter = typeAdapter
    }
    // $COVERAGE-ON$

    typeAdapter
  }

  override val parser: Parser = new DeferredSerializerReference(() => resolved.parser)
  override def materialize[AST](ast: AST)(implicit ops: Ops[AST]) = resolved.materialize(ast)
  override def dematerialize[AST](t: T)(implicit ops: Ops[AST]): AST = resolved.dematerialize(t)
}
