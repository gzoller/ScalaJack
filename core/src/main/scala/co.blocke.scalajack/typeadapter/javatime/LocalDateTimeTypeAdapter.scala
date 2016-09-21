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
        LocalDateTime.parse(reader.readString(), ISO_LOCAL_DATE_TIME)
    }

  override def write(value: LocalDateTime, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value.format(ISO_LOCAL_DATE_TIME))
    }

}
