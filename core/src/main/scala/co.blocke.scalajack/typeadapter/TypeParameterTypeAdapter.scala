package co.blocke.scalajack
package typeadapter

import util.Path
import model._

import scala.collection.mutable.Builder

object TypeParameterTypeAdapterFactory extends TypeAdapterFactory {

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (tt.tpe.typeSymbol.isParameter)
      TypeParameterTypeAdapter[T]()
    else
      next.typeAdapterOf[T]
}

case class TypeParameterTypeAdapter[T]()(implicit tt: TypeTag[T]) extends TypeAdapter[T] {
  def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): T = reader.jackFlavor.anyTypeAdapter.read(path, reader, isMapKey).asInstanceOf[T]
  def write[WIRE](t: T, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = writer.jackFlavor.anyTypeAdapter.write(t, writer, out, isMapKey)
}
