package co.blocke.scalajack.json
package typeadapter

import scala.reflect.runtime.universe.Type

object TypeParameterTypeAdapter extends TypeAdapterFactory {

  override def typeAdapter(tpe: Type, context: Context): Option[TypeAdapter[_]] =
    if (tpe.typeSymbol.isParameter) {
      Some(TypeParameterTypeAdapter(context.typeAdapterOf[Any]))
    } else {
      None
    }
}

case class TypeParameterTypeAdapter[T](anyTypeAdapter: TypeAdapter[Any]) extends TypeAdapter[T] {

  override def read(reader: Reader): T =
    anyTypeAdapter.read(reader).asInstanceOf[T]

  override def write(value: T, writer: Writer): Unit =
    anyTypeAdapter.write(value, writer)

}
