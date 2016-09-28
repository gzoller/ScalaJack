package co.blocke.scalajack
package typeadapter
package javatime

import java.time.format.DateTimeFormatter.ISO_LOCAL_TIME
import java.time.LocalTime

object LocalTimeTypeAdapter extends SimpleTypeAdapter[LocalTime] {

  override val isStringKind: Boolean = true

  override def read(reader: Reader): LocalTime =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.String ⇒
        try {
          LocalTime.parse(reader.readString())
        } catch {
          case dtpe: java.time.format.DateTimeParseException ⇒ throw new java.time.format.DateTimeParseException(dtpe.getMessage + "\n" + reader.showError(), dtpe.getParsedString, dtpe.getErrorIndex)
        }

      case actual ⇒ {
        reader.read()
        throw new IllegalStateException(s"Expected value token of type String, not $actual when reading LocalTime value.  (Is your value wrapped in quotes?)\n" + reader.showError())
      }

    }

  override def write(value: LocalTime, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value.format(ISO_LOCAL_TIME))
    }

}
