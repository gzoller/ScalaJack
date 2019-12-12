package co.blocke.scalajack
package typeadapter

import model._

import scala.collection.mutable
import scala.reflect.runtime.universe.{ NoType, TypeTag, typeOf }
import scala.util.{ Failure, Success, Try }

object TryTypeAdapterFactory extends TypeAdapterFactory {

  override def typeAdapterOf[T](
      next: TypeAdapterFactory
  )(implicit taCache: TypeAdapterCache, tt: TypeTag[T]): TypeAdapter[T] =
    tt.tpe.baseType(typeOf[Try[_]].typeSymbol) match {
      case NoType =>
        next.typeAdapterOf[T]

      case asTry =>
        val valueType = asTry.typeArgs.head
        val valueTypeAdapter = taCache.typeAdapter(valueType)
        TryTypeAdapter(valueTypeAdapter, taCache.jackFlavor)
          .asInstanceOf[TypeAdapter[T]]
    }

}

case class TryTypeAdapter[T](
    valueTypeAdapter: TypeAdapter[T],
    jackFlavor:       JackFlavor[_])
  extends TypeAdapter[Try[T]] {

  def read(parser: Parser): Try[T] = {
    val saved = parser.mark()
    Try { valueTypeAdapter.read(parser) } match {
      case self @ Success(_) => self
      case Failure(cause) =>
        parser.revertToMark(saved)
        Failure(
          new ScalaJackValueError(jackFlavor.anyTypeAdapter.read(parser), cause)
        )
    }
  }

  def write[WIRE](
      t:      Try[T],
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit =
    t match {
      case Success(v) => valueTypeAdapter.write(v, writer, out)
      case Failure(e: ScalaJackValueError) =>
        jackFlavor.anyTypeAdapter.write(e.value, writer, out)
      // $COVERAGE-OFF$Can't test--never called, but compiler requires this case for match completeness
      case Failure(e) => throw e
      // $COVERAGE-ON$
    }
}
