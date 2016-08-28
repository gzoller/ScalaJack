package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{Reader, TokenType, Writer}
import org.joda.time.{DateTime, DateTimeZone}

object JodaDateTimeTypeAdapter extends SimpleTypeAdapter[DateTime] {

  override def read(reader: Reader): DateTime =
    reader.peek match {
      case TokenType.String ⇒
        DateTime.parse(reader.readString()).toDateTime(DateTimeZone.forID("UTC"))

      case TokenType.Number ⇒
        reader.read(expected = TokenType.Number)
        new DateTime(reader.tokenText.toLong).toDateTime(DateTimeZone.forID("UTC"))
    }

  override def write(value: DateTime, writer: Writer): Unit = ???

}
