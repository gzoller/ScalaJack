package co.blocke.scalajack
package typeadapter

import model._

import scala.reflect.runtime.universe._
import scala.collection.mutable

object TypeParameterTypeAdapterFactory extends TypeAdapterFactory {

  override def typeAdapterOf[T](
      next: TypeAdapterFactory
  )(implicit taCache: TypeAdapterCache, tt: TypeTag[T]): TypeAdapter[T] =
    if (tt.tpe.typeSymbol.isParameter)
      TypeParameterTypeAdapter[T](taCache.jackFlavor)
    else
      next.typeAdapterOf[T]
}

case class TypeParameterTypeAdapter[T](jackFlavor: JackFlavor[_])(
    implicit
    tt: TypeTag[T]
) extends TypeAdapter[T] {
  def read(parser: Parser): T =
    jackFlavor.anyTypeAdapter.read(parser).asInstanceOf[T]
  def write[WIRE](
      t:      T,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit =
    jackFlavor.anyTypeAdapter.write(t, writer, out)
}
