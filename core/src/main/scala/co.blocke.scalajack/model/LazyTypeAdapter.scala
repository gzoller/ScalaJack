package co.blocke.scalajack
package model

import scala.reflect.runtime.universe._
import scala.collection.mutable

case class LazyTypeAdapter[T](taCache: TypeAdapterCache, tpe: Type)
  extends TypeAdapter[T] {

  var resolvedTypeAdapter: TypeAdapter[T] = _

  override def resolved: TypeAdapter[T] = {
    var typeAdapter = resolvedTypeAdapter

    // $COVERAGE-OFF$Can't really test as this is triggered by race condition, if it can happen at all.
    if (typeAdapter == null) {
      typeAdapter =
        taCache.typeAdapter(tpe).resolved.asInstanceOf[TypeAdapter[T]]
      if (typeAdapter.isInstanceOf[LazyTypeAdapter[_]]) {
        throw new IllegalStateException(
          s"Type adapter for $tpe is still being built"
        )
      }
      resolvedTypeAdapter = typeAdapter
    }
    // $COVERAGE-ON$

    typeAdapter
  }

  def read(parser: Parser): T = resolved.read(parser)
  def write[WIRE](
      t:      T,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit =
    resolved.write(t, writer, out)
}
