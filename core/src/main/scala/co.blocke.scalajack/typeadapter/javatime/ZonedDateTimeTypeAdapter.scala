package co.blocke.scalajack
package typeadapter
package javatime

import java.time.format.DateTimeFormatter.ISO_ZONED_DATE_TIME
import java.time.ZonedDateTime
import scala.util.{ Try, Success, Failure }

object ZonedDateTimeTypeAdapter extends SimpleTypeAdapter[ZonedDateTime] with StringKind {

  override def read(reader: Reader): ZonedDateTime =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.String ⇒
        Try(ZonedDateTime.parse(reader.readString(), ISO_ZONED_DATE_TIME)) match {
          case Success(u) ⇒ u
          case Failure(u: java.time.format.DateTimeParseException) ⇒ throw new java.time.format.DateTimeParseException(u.getMessage + "\n" + reader.showError(), u.getParsedString, u.getErrorIndex)
          case Failure(u) ⇒ throw new java.lang.IllegalArgumentException(u.getMessage + "\n" + reader.showError())
        }

      case actual ⇒ {
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
