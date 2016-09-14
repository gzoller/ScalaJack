package co.blocke.scalajack.json
package typeadapter
package javatime

import java.time.format.DateTimeFormatter.ISO_ZONED_DATE_TIME
import java.time.ZonedDateTime

object ZonedDateTimeTypeAdapter extends SimpleTypeAdapter[ZonedDateTime] {

  override def read(reader: Reader): ZonedDateTime =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.String ⇒
        ZonedDateTime.parse(reader.readString(), ISO_ZONED_DATE_TIME)
    }

  override def write(value: ZonedDateTime, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value.format(ISO_ZONED_DATE_TIME))
    }

}
