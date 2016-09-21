package co.blocke.scalajack
package typeadapter
package javatime

import java.time.Period

object PeriodTypeAdapter extends SimpleTypeAdapter[Period] {

  override def read(reader: Reader): Period =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.String ⇒
        Period.parse(reader.readString())
    }

  override def write(value: Period, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value.toString)
    }

}
