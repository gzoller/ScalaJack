package co.blocke.scalajack
package typeadapter

import model._

import scala.collection.mutable
import scala.util.{ Failure, Success, Try }

case class FallbackTypeAdapter[A, B <: A](
    attemptedTypeAdapter: Option[TypeAdapter[A]],
    orElseTypeAdapter:    TypeAdapter[B]
) extends TypeAdapter[A] {

  def read(parser: Parser): A = {
    attemptedTypeAdapter match {
      case Some(ata) =>
        val mark = parser.mark()
        Try(ata.read(parser)) match {
          case Success(a) => a
          case Failure(_) =>
            parser.revertToMark(mark)
            orElseTypeAdapter.read(parser)
        }
      // $COVERAGE-OFF$Doesn't ever get called... theoretically not possible but left here for safety (see ClassHelper.applyConcreteTypeMembersToFields)
      case None =>
        orElseTypeAdapter.read(parser)
      // $COVERAGE-ON$
    }
  }

  def write[WIRE](
      t:      A,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit =
    attemptedTypeAdapter match {
      case Some(ta) => ta.write(t, writer, out)
      // $COVERAGE-OFF$Doesn't ever get called... not tested
      case _        => orElseTypeAdapter.write(t.asInstanceOf[B], writer, out)
      // $COVERAGE-ON$
    }
}
