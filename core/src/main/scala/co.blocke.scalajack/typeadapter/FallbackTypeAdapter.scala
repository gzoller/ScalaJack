package co.blocke.scalajack
package typeadapter

import model._

import scala.collection.mutable
import scala.util.{ Failure, Success, Try }
import scala.reflect.runtime.universe.Type

case class FallbackTypeAdapter[A, B <: A](
    taCache:              () => TypeAdapterCache,
    attemptedTypeAdapter: Option[TypeAdapter[A]],
    orElseType:           Type
) extends TypeAdapter[A] {

  def read(parser: Parser): A = {
    attemptedTypeAdapter match {
      case Some(ata) =>
        val mark = parser.mark()
        Try(ata.read(parser)) match {
          case Success(a) => a
          case Failure(_) =>
            parser.revertToMark(mark)
            taCache().typeAdapter(orElseType).read(parser).asInstanceOf[A]
        }
      // $COVERAGE-OFF$Doesn't ever get called... theoretically not possible but left here for safety (see ClassHelper.applyConcreteTypeMembersToFields)
      case None =>
        taCache().typeAdapter(orElseType).read(parser).asInstanceOf[A]
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
      case _ =>
        taCache()
          .typeAdapter(orElseType)
          .asInstanceOf[TypeAdapter[A]]
          .write(t.asInstanceOf[A], writer, out)
      // $COVERAGE-ON$
    }
}
