package co.blocke.scalajack
package typeadapter

import model.{ Transceiver, TypeAdapter }
import util.Path

import scala.collection.mutable.Builder
import scala.util.{ Failure, Success, Try }

case class FallbackTypeAdapter[A, B <: A](attemptedTypeAdapter: TypeAdapter[A], orElseTypeAdapter: TypeAdapter[B]) extends TypeAdapter[A] {

  def read[WIRE](path: Path, reader: Transceiver[WIRE]): A = {
    reader.savePos()
    Try(attemptedTypeAdapter.read(path, reader)) match {
      case Success(a) => a
      case Failure(_) =>
        reader.rollbackToSave()
        orElseTypeAdapter.read(path, reader)
    }
  }

  // Does nothing because writes aren't supported for this TypeAdapter
  def write[WIRE](t: A, writer: Transceiver[WIRE], out: Builder[Any, WIRE]): Unit = {}
}
