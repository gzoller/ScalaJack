package co.blocke.scalajack
package typeadapter
package javatime

import java.time.ZonedDateTime
import java.time.format.{ DateTimeFormatter, DateTimeParseException }

import scala.reflect.runtime.universe.{ Type, typeOf }
import scala.util.{ Failure, Success, Try }

object ZonedDateTimeTypeAdapter extends ZonedDateTimeTypeAdapter(DateTimeFormatter.ISO_ZONED_DATE_TIME)

class ZonedDateTimeTypeAdapter(formatter: DateTimeFormatter) extends TypeAdapter.=:=[ZonedDateTime] with StringKind {

  override object deserializer extends Deserializer[ZonedDateTime] {

    private val ZonedDateTimeType: Type = typeOf[ZonedDateTime]

    override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[ZonedDateTime] =
      json match {
        case JsonString(x) =>
          DeserializationResult(path)(TypeTagged(ZonedDateTime.parse(x, formatter), ZonedDateTimeType), {
            case e: DateTimeParseException =>
              DeserializationError.Malformed(e)
          })
      }

  }

  override def read(reader: Reader): ZonedDateTime =
    reader.peek match {
      case TokenType.String =>
        Try(ZonedDateTime.parse(reader.readString(), formatter)) match {
          case Success(u) => u
          case Failure(u) => throw new DateTimeParseException(u.getMessage + "\n" + reader.showError(), u.asInstanceOf[DateTimeParseException].getParsedString, u.asInstanceOf[DateTimeParseException].getErrorIndex)
        }

      case TokenType.Null =>
        reader.readNull()

      case actual => {
        reader.read()
        throw new IllegalStateException(s"Expected value token of type String, not $actual when reading ZonedDateTime value.  (Is your value wrapped in quotes?)\n" + reader.showError())
      }
    }

  override object serializer extends Serializer[ZonedDateTime] {

    override def serialize[J](tagged: TypeTagged[ZonedDateTime])(implicit ops: JsonOps[J]): SerializationResult[J] =
      tagged match {
        case TypeTagged(null) => SerializationSuccess(JsonNull())
        case TypeTagged(x)    => SerializationSuccess(JsonString(x.format(formatter)))
      }

  }

  override def write(value: ZonedDateTime, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value.format(formatter))
    }

}
