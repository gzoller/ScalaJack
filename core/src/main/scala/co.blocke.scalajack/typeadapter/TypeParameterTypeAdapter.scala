package co.blocke.scalajack
package typeadapter

import util.Path
import model._

import scala.collection.mutable.Builder

object TypeParameterTypeAdapterFactory extends TypeAdapterFactory {

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (tt.tpe.typeSymbol.isParameter)
      TypeParameterTypeAdapter[T](context.typeAdapterOf[Any])
    else
      next.typeAdapterOf[T]
}

case class TypeParameterTypeAdapter[T](anyTypeAdapter: TypeAdapter[Any]) extends TypeAdapter[T] {
  def read[WIRE](path: Path, reader: Transceiver[WIRE]): T = anyTypeAdapter.read(path, reader).asInstanceOf[T]
  def write[WIRE](t: T, writer: Transceiver[WIRE], out: Builder[Any, WIRE], isMapKey: Boolean): Unit = anyTypeAdapter.write(t, writer, out, isMapKey)
}
