package co.blocke.scalajack
package typeadapter
package javatime

import java.time.LocalTime
import java.time.format.{ DateTimeFormatter, DateTimeParseException }

import scala.reflect.runtime.universe.{ Type, typeOf }
import scala.util.{ Failure, Success, Try }

object LocalTimeTypeAdapter extends LocalTimeTypeAdapter(DateTimeFormatter.ISO_LOCAL_TIME) {

  val LocalTimeType: Type = typeOf[LocalTime]

}

class LocalTimeTypeAdapter(formatter: DateTimeFormatter) extends TypeAdapter.=:=[LocalTime] with StringKind {

  override val deserializer: Deserializer[LocalTime] = new LocalTimeDeserializer(formatter)

  override val serializer: Serializer[LocalTime] = new LocalTimeSerializer(formatter)

  override def read(reader: Reader): LocalTime =
    reader.peek match {
      case TokenType.String =>
        Try(LocalTime.parse(reader.readString(), formatter)) match {
          case Success(u) => u
          case Failure(u) => throw new DateTimeParseException(u.getMessage + "\n" + reader.showError(), u.asInstanceOf[DateTimeParseException].getParsedString, u.asInstanceOf[DateTimeParseException].getErrorIndex)
        }

      case TokenType.Null =>
        reader.readNull()

      case actual =>
        reader.read()
        throw new IllegalStateException(s"Expected value token of type String, not $actual when reading LocalTime value.  (Is your value wrapped in quotes?)\n" + reader.showError())

    }

  override def write(value: LocalTime, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value.format(formatter))
    }

}
