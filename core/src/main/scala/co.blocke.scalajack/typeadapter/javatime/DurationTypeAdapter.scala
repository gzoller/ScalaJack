package co.blocke.scalajack
package typeadapter
package javatime

import java.time.Duration
import java.time.format.DateTimeParseException

import scala.util.{ Failure, Success, Try }

object DurationTypeAdapter extends TypeAdapter.=:=[Duration] with StringKind {

  override val deserializer: Deserializer[Duration] = new DurationDeserializer

  override val serializer: Serializer[Duration] = new DurationSerializer

  override def read(reader: Reader): Duration =
    reader.peek match {
      case TokenType.String =>
        Try(Duration.parse(reader.readString())) match {
          case Success(u) => u
          case Failure(u) => throw new DateTimeParseException(u.getMessage + "\n" + reader.showError(), u.asInstanceOf[DateTimeParseException].getParsedString, u.asInstanceOf[DateTimeParseException].getErrorIndex)
        }

      case TokenType.Null =>
        reader.readNull()

      case actual =>
        reader.read()
        throw new IllegalStateException(s"Expected value token of type String, not $actual when reading Duration value.  (Is your value wrapped in quotes?)\n" + reader.showError())
    }

  override def write(value: Duration, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value.toString)
    }

}
