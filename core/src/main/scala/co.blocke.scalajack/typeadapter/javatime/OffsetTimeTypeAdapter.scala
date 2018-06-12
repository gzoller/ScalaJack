package co.blocke.scalajack
package typeadapter
package javatime

import java.time.OffsetTime
import java.time.format.{ DateTimeFormatter, DateTimeParseException }

import scala.util.{ Failure, Success, Try }

object OffsetTimeTypeAdapter extends OffsetTimeTypeAdapter(DateTimeFormatter.ISO_OFFSET_TIME)

class OffsetTimeTypeAdapter(formatter: DateTimeFormatter) extends TypeAdapter.=:=[OffsetTime] with StringKind {

  override val deserializer: Deserializer[OffsetTime] = new OffsetTimeDeserializer(formatter)

  override val serializer: Serializer[OffsetTime] = new OffsetTimeSerializer(formatter)

  override def read(reader: Reader): OffsetTime =
    reader.peek match {
      case TokenType.String =>
        Try(OffsetTime.parse(reader.readString(), formatter)) match {
          case Success(u) => u
          case Failure(u) => throw new DateTimeParseException(u.getMessage + "\n" + reader.showError(), u.asInstanceOf[DateTimeParseException].getParsedString, u.asInstanceOf[DateTimeParseException].getErrorIndex)
        }

      case TokenType.Null =>
        reader.readNull()

      case actual =>
        reader.read()
        throw new IllegalStateException(s"Expected value token of type String, not $actual when reading OffsetTime value.  (Is your value wrapped in quotes?)\n" + reader.showError())
    }

  override def write(value: OffsetTime, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value.format(formatter))
    }

}
