package co.blocke.scalajack
package typeadapter
package javatime

import java.time.format.DateTimeFormatter.ISO_OFFSET_DATE_TIME
import java.time.format.DateTimeParseException
import java.time.OffsetDateTime
import scala.util.{ Try, Success, Failure }

object OffsetDateTimeTypeAdapter extends SimpleTypeAdapter[OffsetDateTime] with StringKind {

  override def read(reader: Reader): OffsetDateTime =
    reader.peek match {
      case TokenType.String =>
        Try(OffsetDateTime.parse(reader.readString(), ISO_OFFSET_DATE_TIME)) match {
          case Success(u) => u
          case Failure(u) => throw new DateTimeParseException(u.getMessage + "\n" + reader.showError(), u.asInstanceOf[DateTimeParseException].getParsedString, u.asInstanceOf[DateTimeParseException].getErrorIndex)
        }

      case TokenType.Null =>
        reader.readNull()

      case actual => {
        reader.read()
        throw new IllegalStateException(s"Expected value token of type String, not $actual when reading OffsetDateTime value.  (Is your value wrapped in quotes?)\n" + reader.showError())
      }

    }

  override def write(value: OffsetDateTime, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value.format(ISO_OFFSET_DATE_TIME))
    }

}
