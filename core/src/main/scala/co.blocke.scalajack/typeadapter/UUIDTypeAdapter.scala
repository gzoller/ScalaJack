package co.blocke.scalajack
package typeadapter

import model._
import java.util.UUID

import scala.collection.mutable
import scala.util.{ Failure, Success, Try }

object UUIDTypeAdapterFactory extends TypeAdapter.=:=[UUID] with Stringish {
  def read(parser: Parser): UUID = {
    val u = parser.expectString()
    if (u == null)
      null
    else {
      Try(UUID.fromString(u)) match {
        case Success(uuid) => uuid
        case Failure(uuid) =>
          parser.backspace()
          throw new ScalaJackError(
            parser
              .showError(s"Failed to create UUID value from parsed text ${u}")
          )
      }
    }
  }

  def write[WIRE](
      t:      UUID,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit = t match {
    case null => writer.writeNull(out)
    case _    => writer.writeString(t.toString, out)
  }
}
