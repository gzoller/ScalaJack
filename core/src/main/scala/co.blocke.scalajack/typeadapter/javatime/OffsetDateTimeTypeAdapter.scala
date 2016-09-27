package co.blocke.scalajack
package typeadapter
package javatime

import java.time.format.DateTimeFormatter.ISO_OFFSET_DATE_TIME
import java.time.OffsetDateTime

object OffsetDateTimeTypeAdapter extends SimpleTypeAdapter[OffsetDateTime] {

  override def read(reader: Reader): OffsetDateTime =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.String ⇒
        try {
          OffsetDateTime.parse(reader.readString(), ISO_OFFSET_DATE_TIME)
        } catch {
          case dtpe: java.time.format.DateTimeParseException ⇒ throw new java.time.format.DateTimeParseException(dtpe.getMessage + "\n" + reader.showError(), dtpe.getParsedString, dtpe.getErrorIndex)
        }

      case actual ⇒ {
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
