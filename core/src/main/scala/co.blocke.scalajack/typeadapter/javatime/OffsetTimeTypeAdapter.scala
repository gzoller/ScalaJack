package co.blocke.scalajack
package typeadapter
package javatime

import java.time.format.DateTimeFormatter.ISO_OFFSET_TIME
import java.time.format.DateTimeParseException
import java.time.OffsetTime
import scala.util.{ Try, Success, Failure }

object OffsetTimeTypeAdapter extends SimpleTypeAdapter[OffsetTime] with StringKind {

  override def read(reader: Reader): OffsetTime =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.String ⇒
        Try(OffsetTime.parse(reader.readString())) match {
          case Success(u) ⇒ u
          case Failure(u) ⇒ throw new DateTimeParseException(u.getMessage + "\n" + reader.showError(), u.asInstanceOf[DateTimeParseException].getParsedString, u.asInstanceOf[DateTimeParseException].getErrorIndex)
        }

      case actual ⇒ {
        reader.read()
        throw new IllegalStateException(s"Expected value token of type String, not $actual when reading OffsetTime value.  (Is your value wrapped in quotes?)\n" + reader.showError())
      }

    }

  override def write(value: OffsetTime, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value.format(ISO_OFFSET_TIME))
    }

}
