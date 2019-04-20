package co.blocke.series5
package typeadapter
package javatime

import java.time.format.DateTimeFormatter.ISO_ZONED_DATE_TIME
import java.time.format.DateTimeParseException
import java.time.ZonedDateTime
import scala.util.{ Try, Success, Failure }

object ZonedDateTimeTypeAdapter extends SimpleTypeAdapter[ZonedDateTime] with StringKind {

  override def read(reader: Reader): ZonedDateTime =
    reader.peek match {
      case TokenType.String =>
        Try(ZonedDateTime.parse(reader.readString(), ISO_ZONED_DATE_TIME)) match {
          case Success(u) => u
          case Failure(u) => throw new DateTimeParseException(u.getMessage + "\n" + reader.showError(), u.asInstanceOf[DateTimeParseException].getParsedString, u.asInstanceOf[DateTimeParseException].getErrorIndex)
        }

      case TokenType.Null =>
        reader.readNull()

      case actual => {
        reader.read()
        throw new IllegalStateException(s"Expected value token of type String, not $actual when reading ZonedDateTime value.  (Is your value wrapped in quotes?)\n" + reader.showError())
      }
    }

  override def write(value: ZonedDateTime, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value.format(ISO_ZONED_DATE_TIME))
    }

}
