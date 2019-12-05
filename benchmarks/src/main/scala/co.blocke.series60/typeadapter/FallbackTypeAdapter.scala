package co.blocke.series60
package typeadapter

import model._
import util.Path

import scala.collection.mutable.Builder
import scala.util.{ Failure, Success, Try }

case class FallbackTypeAdapter[A, B <: A](attemptedTypeAdapter: Option[TypeAdapter[A]], orElseTypeAdapter: TypeAdapter[B]) extends TypeAdapter[A] {

  def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): A = {
    attemptedTypeAdapter match {
      case Some(ata) =>
        val savedReader = reader.copy
        Try(ata.read(path, reader, isMapKey)) match {
          case Success(a) => a
          case Failure(_) =>
            reader.syncPositionTo(savedReader)
            orElseTypeAdapter.read(path, reader, isMapKey)
        }
      // $COVERAGE-OFF$Doesn't ever get called... theoretically not possible but left here for safety (see ClassHelper.applyConcreteTypeMembersToFields)
      case None =>
        orElseTypeAdapter.read(path, reader, isMapKey)
      // $COVERAGE-ON$
    }
  }

  // $COVERAGE-OFF$Doesn't ever get called... not tested
  def write[WIRE](t: A, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = {}
  // $COVERAGE-ON$
}
