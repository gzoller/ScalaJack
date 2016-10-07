package co.blocke.scalajack
package typeadapter
package javatime

import java.time.format.DateTimeFormatter.ISO_LOCAL_DATE
import java.time.LocalDate
import scala.util.{ Try, Success, Failure }

object LocalDateTypeAdapter extends SimpleTypeAdapter[LocalDate] with StringKind {

  override def read(reader: Reader): LocalDate =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.String ⇒
        Try(LocalDate.parse(reader.readString())) match {
          case Success(u) ⇒ u
          case Failure(u: java.time.format.DateTimeParseException) ⇒ throw new java.time.format.DateTimeParseException(u.getMessage + "\n" + reader.showError(), u.getParsedString, u.getErrorIndex)
          case Failure(u) ⇒ throw new java.lang.IllegalArgumentException(u.getMessage + "\n" + reader.showError())
        }

      case actual ⇒ {
        reader.read()
        throw new IllegalStateException(s"Expected value token of type String, not $actual when reading LocalDate value.  (Is your value wrapped in quotes?)\n" + reader.showError())
      }
    }

  override def write(value: LocalDate, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value.format(ISO_LOCAL_DATE))
    }

}
