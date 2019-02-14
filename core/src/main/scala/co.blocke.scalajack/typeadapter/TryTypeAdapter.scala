package co.blocke.scalajack
package typeadapter

import model._
import util.Path

import scala.collection.mutable.Builder
import scala.reflect.runtime.universe.{ NoType, TypeTag, typeOf }
import scala.util.{ Failure, Success, Try }

class ValueBackedException(val value: Any, val throwable: Throwable) extends Exception(throwable.getMessage())

object TryTypeAdapterFactory extends TypeAdapterFactory {

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    tt.tpe.baseType(typeOf[Try[_]].typeSymbol) match {
      case NoType =>
        next.typeAdapterOf[T]

      case asTry =>
        val valueType :: Nil = asTry.typeArgs
        val valueTypeAdapter = context.typeAdapter(valueType)
        TryTypeAdapter(valueTypeAdapter, context.typeAdapterOf[Any]).asInstanceOf[TypeAdapter[T]]
    }

}

case class TryTypeAdapter[T](valueTypeAdapter: TypeAdapter[T], anyTypeAdapter: TypeAdapter[Any]) extends TypeAdapter[Try[T]] {

  def read[WIRE](path: Path, reader: Transceiver[WIRE]): Try[T] = {
    reader.savePos()
    Try { valueTypeAdapter.read(path, reader) } match {
      case self @ Success(_) =>
        self

      case Failure(cause) =>
        reader.rollbackToSave()
        Failure(new ValueBackedException(anyTypeAdapter.read(path, reader), cause))
    }
  }

  def write[WIRE](t: Try[T], writer: Transceiver[WIRE], out: Builder[Any, WIRE], isMapKey: Boolean): Unit =
    t match {
      case Success(v)                       => valueTypeAdapter.write(v, writer, out, isMapKey)
      case Failure(e: ValueBackedException) => anyTypeAdapter.write(e.value, writer, out, isMapKey)
      case Failure(e)                       => throw e
    }
}
