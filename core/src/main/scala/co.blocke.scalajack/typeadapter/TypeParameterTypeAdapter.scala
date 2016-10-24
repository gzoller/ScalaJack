package co.blocke.scalajack
package typeadapter

import scala.reflect.runtime.universe.TypeTag

object TypeParameterTypeAdapter extends TypeAdapterFactory {

  override def typeAdapterOf[T](context: Context, next: TypeAdapterFactory)(implicit tt: TypeTag[T]): TypeAdapter[T] =
    if (tt.tpe.typeSymbol.isParameter) {
      TypeParameterTypeAdapter(context.typeAdapterOf[Any])
    } else {
      next.typeAdapterOf[T](context)
    }
}

case class TypeParameterTypeAdapter[T](anyTypeAdapter: TypeAdapter[Any]) extends TypeAdapter[T] {

  override def read(reader: Reader): T =
    anyTypeAdapter.read(reader).asInstanceOf[T]

  override def write(value: T, writer: Writer): Unit =
    anyTypeAdapter.write(value, writer)

}
