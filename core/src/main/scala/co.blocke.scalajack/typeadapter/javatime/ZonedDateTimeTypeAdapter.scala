package co.blocke.scalajack
package typeadapter
package javatime

import java.time.format.DateTimeFormatter.ISO_ZONED_DATE_TIME
import java.time.ZonedDateTime

object ZonedDateTimeTypeAdapter extends SimpleTypeAdapter[ZonedDateTime] with StringKind {

  override def read(reader: Reader): ZonedDateTime =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.String ⇒
        try {
          ZonedDateTime.parse(reader.readString(), ISO_ZONED_DATE_TIME)
        } catch {
          case dtpe: java.time.format.DateTimeParseException ⇒ throw new java.time.format.DateTimeParseException(dtpe.getMessage + "\n" + reader.showError(), dtpe.getParsedString, dtpe.getErrorIndex)
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
