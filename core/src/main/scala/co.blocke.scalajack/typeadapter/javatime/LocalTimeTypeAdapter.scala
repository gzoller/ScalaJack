package co.blocke.scalajack
package typeadapter
package javatime

import java.time.LocalTime
import java.time.format.{ DateTimeFormatter, DateTimeParseException }

import scala.reflect.runtime.universe.{ Type, typeOf }
import scala.util.{ Failure, Success, Try }

object LocalTimeTypeAdapter extends LocalTimeTypeAdapter(DateTimeFormatter.ISO_LOCAL_TIME)

class LocalTimeTypeAdapter(formatter: DateTimeFormatter) extends TypeAdapter.=:=[LocalTime] with StringKind {

  override object deserializer extends Deserializer[LocalTime] {

    private val LocalTimeType: Type = typeOf[LocalTime]

    override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[LocalTime] =
      json match {
        case JsonString(x) =>
          DeserializationResult(path)(TypeTagged(LocalTime.parse(x, formatter), LocalTimeType), {
            case e: DateTimeParseException =>
              DeserializationError.Malformed(e)
          })

        case JsonNull() =>
          DeserializationSuccess(TypeTagged(null, LocalTimeType))

        case _ =>
          DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON string"))
      }

  }

  override def read(reader: Reader): LocalTime =
    reader.peek match {
      case TokenType.String =>
        Try(LocalTime.parse(reader.readString(), formatter)) match {
          case Success(u) => u
          case Failure(u) => throw new DateTimeParseException(u.getMessage + "\n" + reader.showError(), u.asInstanceOf[DateTimeParseException].getParsedString, u.asInstanceOf[DateTimeParseException].getErrorIndex)
        }

      case TokenType.Null =>
        reader.readNull()

      case actual => {
        reader.read()
        throw new IllegalStateException(s"Expected value token of type String, not $actual when reading LocalTime value.  (Is your value wrapped in quotes?)\n" + reader.showError())
      }

    }

  override object serializer extends Serializer[LocalTime] {

    override def serialize[J](tagged: TypeTagged[LocalTime])(implicit ops: JsonOps[J]): SerializationResult[J] =
      tagged match {
        case TypeTagged(null) => SerializationSuccess(JsonNull())
        case TypeTagged(x)    => SerializationSuccess(JsonString(x.format(formatter)))
      }

  }

  override def write(value: LocalTime, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value.format(formatter))
    }

}
