package co.blocke.scalajack
package typeadapter

import model._
import util.Path

import scala.collection.mutable.Builder
import scala.util.{ Failure, Success, Try }

case class FallbackTypeAdapter[A, B <: A](attemptedTypeAdapter: Option[TypeAdapter[A]], orElseTypeAdapter: TypeAdapter[B]) extends TypeAdapter[A] {

  def read[WIRE](path: Path, reader: Reader[WIRE]): A = {
    attemptedTypeAdapter match {
      case Some(ata) =>
        val savedReader = reader.copy
        Try(ata.read(path, reader)) match {
          case Success(a) => a
          case Failure(_) =>
            reader.syncPositionTo(savedReader)
            orElseTypeAdapter.read(path, reader)
        }
      case None =>
        orElseTypeAdapter.read(path, reader)
    }
  }

  // $COVERAGE-OFF$Doesn't ever get called... not tested
  def write[WIRE](t: A, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = {}
  // $COVERAGE-ON$
}
