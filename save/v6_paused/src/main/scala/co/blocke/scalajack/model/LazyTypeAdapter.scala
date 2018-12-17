package co.blocke.scalajack
package model

import util.Path

case class LazyTypeAdapter[J](context: Context, tpe: Type) extends TypeAdapter {

  type T = J

  var resolvedTypeAdapter: TypeAdapter = _

  override def resolved: TypeAdapter = {
    var typeAdapter = resolvedTypeAdapter

    // $COVERAGE-OFF$Can't really test as this is triggered by race condition, if it can happen at all.
    if (typeAdapter == null) {
      typeAdapter = context.typeAdapter(tpe).resolved
      if (typeAdapter.isInstanceOf[LazyTypeAdapter]) {
        throw new IllegalStateException(s"Type adapter for $tpe is still being built")
      }
      resolvedTypeAdapter = typeAdapter
    }
    // $COVERAGE-ON$

    typeAdapter
  }

  override def read[AST](path: Path, ast: AST)(implicit ops: Ops[AST], g: SerializationGuidance) = resolved.read(path, ast)
  override def write[AST](t: T)(implicit ops: Ops[AST], g: SerializationGuidance): AST = resolved.write(t)
}
