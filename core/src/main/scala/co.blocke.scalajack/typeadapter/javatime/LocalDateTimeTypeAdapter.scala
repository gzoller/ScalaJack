package co.blocke.scalajack
package typeadapter
package javatime

import java.time.LocalDateTime
import java.time.format.{ DateTimeFormatter, DateTimeParseException }

import scala.util.{ Failure, Success, Try }

object LocalDateTimeTypeAdapter extends LocalDateTimeTypeAdapter(DateTimeFormatter.ISO_LOCAL_DATE_TIME)

class LocalDateTimeTypeAdapter(formatter: DateTimeFormatter) extends TypeAdapter.=:=[LocalDateTime] with StringKind {

  override val deserializer: Deserializer[LocalDateTime] = new LocalDateTimeDeserializer(formatter)

  override val serializer: Serializer[LocalDateTime] = new LocalDateTimeSerializer(formatter)

  override def read(reader: Reader): LocalDateTime =
    reader.peek match {
      case TokenType.String =>
        Try(LocalDateTime.parse(reader.readString(), formatter)) match {
          case Success(u) => u
          case Failure(u) => throw new DateTimeParseException(u.getMessage + "\n" + reader.showError(), u.asInstanceOf[DateTimeParseException].getParsedString, u.asInstanceOf[DateTimeParseException].getErrorIndex)
        }

      case TokenType.Null =>
        reader.readNull()

      case actual =>
        reader.read()
        throw new IllegalStateException(s"Expected value token of type String, not $actual when reading LocalDateTime value.  (Is your value wrapped in quotes?)\n" + reader.showError())
    }

  override def write(value: LocalDateTime, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value.format(formatter))
    }

}
