package co.blocke.scalajack
package typeadapter

import java.util.UUID
import scala.util.{ Try, Success, Failure }

object UUIDTypeAdapter extends SimpleTypeAdapter[UUID] {

  override def read(reader: Reader): UUID =
    reader.peek match {
      case TokenType.String =>
        Try(UUID.fromString(reader.readString())) match {
          case Success(u) => u
          case Failure(u) => throw new java.lang.IllegalArgumentException(u.getMessage + "\n" + reader.showError())
        }

      case TokenType.Null =>
        reader.readNull()

      case actual => {
        reader.read()
        throw new IllegalStateException(s"Expected value token of type String, not $actual when reading UUID value.\n" + reader.showError())
      }
    }

  override def write(value: UUID, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value.toString)
    }

}
