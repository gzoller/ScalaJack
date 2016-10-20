package co.blocke.scalajack
package typeadapter
package javatime

import java.time.format.DateTimeFormatter.ISO_LOCAL_DATE_TIME
import java.time.format.DateTimeParseException
import java.time.LocalDateTime
import scala.util.{ Try, Success, Failure }

object LocalDateTimeTypeAdapter extends SimpleTypeAdapter[LocalDateTime] with StringKind {

  override def read(reader: Reader): LocalDateTime =
    reader.peek match {
      case TokenType.String ⇒
        Try(LocalDateTime.parse(reader.readString(), ISO_LOCAL_DATE_TIME)) match {
          case Success(u) ⇒ u
          case Failure(u) ⇒ throw new DateTimeParseException(u.getMessage + "\n" + reader.showError(), u.asInstanceOf[DateTimeParseException].getParsedString, u.asInstanceOf[DateTimeParseException].getErrorIndex)
        }

      case TokenType.Null ⇒
        reader.readNull()

      case actual ⇒ {
        reader.read()
        throw new IllegalStateException(s"Expected value token of type String, not $actual when reading LocalDateTime value.  (Is your value wrapped in quotes?)\n" + reader.showError())
      }
    }

  override def write(value: LocalDateTime, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value.format(ISO_LOCAL_DATE_TIME))
    }

}
