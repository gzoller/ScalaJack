package co.blocke.scalajack
package typeadapter

import model.{ Transceiver, TypeAdapter }
import util.Path

import scala.collection.mutable.Builder
import scala.util.{ Failure, Success, Try }

//case class FallbackTypeAdapter[A, B <: A](attemptedTypeAdapter: Option[TypeAdapter[A]], orElseTypeAdapter: TypeAdapter[B]) extends TypeAdapter[A] {
case class FallbackTypeAdapter[A, B <: A](attemptedTypeAdapter: Option[TypeAdapter[A]], orElseTypeAdapter: TypeAdapter[B]) extends TypeAdapter[A] {

  def read[WIRE](path: Path, reader: Transceiver[WIRE]): A = {
    reader.savePos()
    attemptedTypeAdapter match {
      case Some(ata) =>
        Try(ata.read(path, reader)) match {
          case Success(a) => a
          case Failure(_) =>
            reader.rollbackToSave()
            orElseTypeAdapter.read(path, reader)
        }
      case None =>
        orElseTypeAdapter.read(path, reader)
    }
  }

  // $COVERAGE-OFF$Doesn't ever get called... not tested
  def write[WIRE](t: A, writer: Transceiver[WIRE], out: Builder[Any, WIRE], isMapKey: Boolean): Unit = {}
  // $COVERAGE-ON$
}
