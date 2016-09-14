package co.blocke.scalajack.json
package typeadapter
package joda

import org.joda.time.{ DateTime, DateTimeZone }

object JodaDateTimeTypeAdapter extends SimpleTypeAdapter[DateTime] {

  override def read(reader: Reader): DateTime =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.String ⇒
        DateTime.parse(reader.readString()).toDateTime(DateTimeZone.forID("UTC"))

      case TokenType.Number ⇒
        reader.read(expected = TokenType.Number)
        new DateTime(reader.tokenText.toLong).toDateTime(DateTimeZone.forID("UTC"))
    }

  override def write(dateTime: DateTime, writer: Writer): Unit =
    if (dateTime == null) {
      writer.writeNull()
    } else {
      // TODO Why do we default to UNIX timestamps instead of the ISO-formatted date?
      writer.writeLong(dateTime.getMillis)
    }

}
