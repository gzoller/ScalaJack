package co.blocke.scalajack

import scala.reflect.runtime.universe.Type

case class LazyTypeAdapter[T](context: Context, tpe: Type) extends TypeAdapter[T] {

  var resolvedTypeAdapter: TypeAdapter[T] = _

  override def resolved: TypeAdapter[T] = {
    var typeAdapter = resolvedTypeAdapter

    if (typeAdapter == null) {
      typeAdapter = context.typeAdapter(tpe).asInstanceOf[TypeAdapter[T]]
      if (typeAdapter.isInstanceOf[LazyTypeAdapter[_]]) {
        throw new IllegalStateException(s"Type adapter for $tpe is still being built")
      }

      resolvedTypeAdapter = typeAdapter
    }

    typeAdapter
  }

  override def read(reader: Reader): T =
    resolved.read(reader)

  override def write(value: T, writer: Writer): Unit =
    resolved.write(value, writer)

}
