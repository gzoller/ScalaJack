package co.blocke.scalajack.json
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
        OffsetDateTime.parse(reader.readString(), ISO_OFFSET_DATE_TIME)
    }

  override def write(value: OffsetDateTime, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value.format(ISO_OFFSET_DATE_TIME))
    }

}
