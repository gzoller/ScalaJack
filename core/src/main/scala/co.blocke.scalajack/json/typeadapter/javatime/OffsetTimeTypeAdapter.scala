package co.blocke.scalajack.json
package typeadapter
package javatime

import java.time.format.DateTimeFormatter.ISO_OFFSET_TIME
import java.time.OffsetTime

object OffsetTimeTypeAdapter extends SimpleTypeAdapter[OffsetTime] {

  override def read(reader: Reader): OffsetTime =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.String ⇒
        OffsetTime.parse(reader.readString(), ISO_OFFSET_TIME)
    }

  override def write(value: OffsetTime, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value.format(ISO_OFFSET_TIME))
    }

}
