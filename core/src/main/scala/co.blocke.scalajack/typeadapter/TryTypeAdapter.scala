package co.blocke.scalajack
package typeadapter

import model._
import util.Path

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

  def read(path: Path, reader: Reader, isMapKey: Boolean): Try[T] = {
    reader.savePos()
    Try { valueTypeAdapter.read(path, reader, isMapKey) } match {
      case self @ Success(_) =>
        self

      case Failure(cause) =>
        reader.rollbackToSave()
        throw cause
      //        Failure(new UnreadableException(reader.captureValue(), cause))
    }
  }

  /*
  override def write(value: Try[T], writer: Writer): Unit =
    value match {
      case Success(v) =>
        valueTypeAdapter.write(v, writer)

      case Failure(e: UnreadableException) =>
        e.write(writer)

      case Failure(e) =>
        // $COVERAGE-OFF$USafety catch--shouldn't be possible
        throw e
      // $COVERAGE-ON$
    }
  */
}
