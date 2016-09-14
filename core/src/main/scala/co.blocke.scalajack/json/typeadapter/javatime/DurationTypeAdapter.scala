package co.blocke.scalajack.json
package typeadapter
package javatime

import java.time.Duration

object DurationTypeAdapter extends SimpleTypeAdapter[Duration] {

  override def read(reader: Reader): Duration =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.String ⇒
        Duration.parse(reader.readString())
    }

  override def write(value: Duration, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value.toString)
    }

}
