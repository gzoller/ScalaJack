package co.blocke.scalajack
package typeadapter

import model._

import scala.collection.mutable
import co.blocke.scala_reflection._
import co.blocke.scala_reflection.info._
import scala.util.{Try, Success, Failure}


object TryTypeAdapterFactory extends TypeAdapterFactory:
  def matches(concrete: RType): Boolean = 
    concrete match {
      case _: TryInfo => true
      case _ => false
    }
  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[_] =
    TryTypeAdapter(concrete, taCache.typeAdapterOf(concrete.asInstanceOf[TryInfo].tryType), taCache.jackFlavor)


case class TryTypeAdapter[T](
    info:             RType,
    valueTypeAdapter: TypeAdapter[T],
    jackFlavor:       JackFlavor[_]
  ) extends TypeAdapter[Try[T]]:

  def read(parser: Parser): Try[T] =
    val saved = parser.mark()
    Try { valueTypeAdapter.read(parser) } match {
      case self @ Success(_) => self
      case Failure(cause) =>
        parser.revertToMark(saved)
        val f = Failure(
          new ScalaJackValueError(jackFlavor.anyTypeAdapter.read(parser), cause)
        )
        f
    }

  def write[WIRE](
      t:      Try[T],
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit =
    t match {
      case Success(v) => valueTypeAdapter.write(v, writer, out)
      case Failure(e: ScalaJackValueError) => jackFlavor.anyTypeAdapter.write(e.value, writer, out)
      case Failure(e) => throw e
    }
