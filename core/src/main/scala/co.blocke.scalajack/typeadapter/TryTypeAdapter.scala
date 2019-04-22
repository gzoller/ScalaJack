package co.blocke.scalajack
package typeadapter

import model._
import util.Path

import scala.collection.mutable.Builder
import scala.reflect.runtime.universe.{ NoType, TypeTag, typeOf }
import scala.util.{ Failure, Success, Try }

class ValueBackedException(val value: Any, val throwable: Throwable) extends SJError(throwable.getMessage())

object TryTypeAdapterFactory extends TypeAdapterFactory {

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    tt.tpe.baseType(typeOf[Try[_]].typeSymbol) match {
      case NoType =>
        next.typeAdapterOf[T]

      case asTry =>
        val valueType :: Nil = asTry.typeArgs
        val valueTypeAdapter = context.typeAdapter(valueType)
        TryTypeAdapter(valueTypeAdapter).asInstanceOf[TypeAdapter[T]]
    }

}

case class TryTypeAdapter[T](valueTypeAdapter: TypeAdapter[T]) extends TypeAdapter[Try[T]] {

  def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): Try[T] = {
    val savedReader = reader.copy
    Try { valueTypeAdapter.read(path, reader, isMapKey) } match {
      case self @ Success(_) =>
        self

      case Failure(cause) =>
        reader.syncPositionTo(savedReader)
        Failure(new ValueBackedException(reader.jackFlavor.anyTypeAdapter.read(path, reader), cause))
    }
  }

  def write[WIRE](t: Try[T], writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit =
    t match {
      case Success(v)                       => valueTypeAdapter.write(v, writer, out, isMapKey)
      case Failure(e: ValueBackedException) => writer.jackFlavor.anyTypeAdapter.write(e.value, writer, out, isMapKey)
      // $COVERAGE-OFF$Can't test--never called, but compiler requires this case for match completeness
      case Failure(e)                       => throw e
      // $COVERAGE-ON$
    }
}
