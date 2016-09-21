package co.blocke.scalajack
package typeadapter
package javatime

import java.time.format.DateTimeFormatter.ISO_LOCAL_TIME
import java.time.LocalTime

object LocalTimeTypeAdapter extends SimpleTypeAdapter[LocalTime] {

  override def read(reader: Reader): LocalTime =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.String ⇒
        LocalTime.parse(reader.readString(), ISO_LOCAL_TIME)
    }

  override def write(value: LocalTime, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value.format(ISO_LOCAL_TIME))
    }

}
