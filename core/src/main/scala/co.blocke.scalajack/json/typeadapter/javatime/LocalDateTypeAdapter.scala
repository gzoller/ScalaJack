package co.blocke.scalajack.json
package typeadapter
package javatime

import java.time.format.DateTimeFormatter.ISO_LOCAL_DATE
import java.time.LocalDate

object LocalDateTypeAdapter extends SimpleTypeAdapter[LocalDate] {

  override def read(reader: Reader): LocalDate =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.String ⇒
        LocalDate.parse(reader.readString(), ISO_LOCAL_DATE)
    }

  override def write(value: LocalDate, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value.format(ISO_LOCAL_DATE))
    }

}
