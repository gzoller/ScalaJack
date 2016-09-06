package co.blocke.scalajack.flexjson

import scala.reflect.runtime.universe.Type

case class LazyTypeAdapter[T](context: Context, tpe: Type) extends TypeAdapter[T] {

  var resolvedTypeAdapter: TypeAdapter[T] = _

  @inline def resolve(): TypeAdapter[T] = {
    var typeAdapter = resolvedTypeAdapter

    if (typeAdapter == null) {
      typeAdapter = context.typeAdapter(tpe, tpe.typeArgs).asInstanceOf[TypeAdapter[T]]
      if (typeAdapter.isInstanceOf[LazyTypeAdapter[_]]) {
        println("Looking for " + tpe)
        throw new IllegalStateException(s"Type adapter for $tpe is still being built")
      }

      resolvedTypeAdapter = typeAdapter
    }

    typeAdapter
  }

  override def read(reader: Reader): T =
    resolve().read(reader)

  override def write(value: T, writer: Writer): Unit =
    resolve().write(value, writer)

}
