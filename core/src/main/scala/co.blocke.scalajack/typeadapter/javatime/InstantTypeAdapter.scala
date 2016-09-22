package co.blocke.scalajack
package typeadapter
package javatime

import java.time.Instant

object InstantTypeAdapter extends SimpleTypeAdapter[Instant] {

  override def read(reader: Reader): Instant =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.String ⇒
        Instant.parse(reader.readString())
    }

  override def write(value: Instant, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value.toString)
    }

}
