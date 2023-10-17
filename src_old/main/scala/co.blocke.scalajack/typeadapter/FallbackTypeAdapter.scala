package co.blocke.scalajack
package typeadapter

import model._
import co.blocke.scala_reflection.RType

import scala.collection.mutable
import scala.util.{ Failure, Success, Try }

case class FallbackTypeAdapter[A, B](
    attemptedTypeAdapter: TypeAdapter[A],
    orElseTypeAdapter:    TypeAdapter[B]
  ) extends TypeAdapter[A]:

  val info: RType = attemptedTypeAdapter.info

  def read(parser: Parser): A =
    val mark = parser.mark()
    Try(attemptedTypeAdapter.read(parser)) match {
      case Success(a) => a
      case Failure(_) =>
        parser.revertToMark(mark)
        orElseTypeAdapter.read(parser).asInstanceOf[A]
    }

  def write[WIRE](
      t:      A,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit =
    attemptedTypeAdapter.write(t, writer, out)
