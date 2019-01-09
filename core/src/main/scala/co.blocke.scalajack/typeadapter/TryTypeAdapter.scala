package co.blocke.scalajack
package typeadapter

import model._
import util.Path

import scala.collection.mutable.Builder
import scala.reflect.runtime.universe.{ NoType, TypeTag, typeOf }
import scala.util.{ Failure, Success, Try }

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

  def read(path: Path, reader: Transceiver, isMapKey: Boolean): Try[T] = {
    reader.savePos()
    Try { valueTypeAdapter.read(path, reader, isMapKey) } match {
      case self @ Success(_) =>
        self

      case Failure(cause) =>
        reader.rollbackToSave()
        throw new SJReadError(path, Invalid, s"Reading Try type failed", List.empty[String], Some(cause))
    }
  }

  def write(t: Try[T], writer: Transceiver)(out: Builder[Any, writer.WIRE]): Unit =
    t match {
      case Success(v) => valueTypeAdapter.write(v, writer)(out)
      case Failure(e) => throw e
    }
}
