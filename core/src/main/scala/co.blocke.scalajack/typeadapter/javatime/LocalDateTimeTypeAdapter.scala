package co.blocke.scalajack
package typeadapter
package javatime

import java.time.format.DateTimeFormatter.ISO_LOCAL_DATE_TIME
import java.time.LocalDateTime

object LocalDateTimeTypeAdapter extends SimpleTypeAdapter[LocalDateTime] {

  override def read(reader: Reader): LocalDateTime =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.String ⇒
        try {
          LocalDateTime.parse(reader.readString(), ISO_LOCAL_DATE_TIME)
        } catch {
          case dtpe: java.time.format.DateTimeParseException ⇒ throw new java.time.format.DateTimeParseException(dtpe.getMessage + "\n" + reader.showError(), dtpe.getParsedString, dtpe.getErrorIndex)
        }

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
