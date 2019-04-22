package co.blocke.scalajack
package typeadapter

import util.Path
import model._
import java.util.UUID

import scala.collection.mutable.Builder
import scala.util.{ Failure, Success, Try }

object UUIDTypeAdapterFactory extends TypeAdapter.=:=[UUID] with Stringish {
  def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): UUID = {
    reader.readString(path) match {
      case null => null
      case s: String =>
        Try(UUID.fromString(s)) match {
          case Success(u) => u
          case Failure(u) =>
            reader.back
            throw new ReadMalformedError(reader.showError(path, s"Failed to create UUID value from parsed text ${s}"))
        }
    }
  }
  def write[WIRE](t: UUID, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = t match {
    case null => writer.writeNull(out)
    case _    => writer.writeString(t.toString, out)
  }
}
