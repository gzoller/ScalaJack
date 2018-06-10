package co.blocke.scalajack
package typeadapter
package javatime

import java.time.OffsetTime
import java.time.format.{ DateTimeFormatter, DateTimeParseException }

import scala.reflect.runtime.universe.{ Type, typeOf }
import scala.util.{ Failure, Success, Try }

object OffsetTimeTypeAdapter extends OffsetTimeTypeAdapter(DateTimeFormatter.ISO_OFFSET_TIME)

class OffsetTimeTypeAdapter(formatter: DateTimeFormatter) extends TypeAdapter.=:=[OffsetTime] with StringKind {

  override object deserializer extends Deserializer[OffsetTime] {

    private val OffsetTimeType: Type = typeOf[OffsetTime]

    override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[OffsetTime] =
      json match {
        case JsonString(x) =>
          DeserializationResult(path)(TypeTagged(OffsetTime.parse(x, formatter), OffsetTimeType), {
            case e: DateTimeParseException =>
              DeserializationError.Malformed(e)
          })

        case JsonNull() =>
          DeserializationSuccess(TypeTagged(null, OffsetTimeType))

        case _ =>
          DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON string"))
      }

  }

  override def read(reader: Reader): OffsetTime =
    reader.peek match {
      case TokenType.String =>
        Try(OffsetTime.parse(reader.readString(), formatter)) match {
          case Success(u) => u
          case Failure(u) => throw new DateTimeParseException(u.getMessage + "\n" + reader.showError(), u.asInstanceOf[DateTimeParseException].getParsedString, u.asInstanceOf[DateTimeParseException].getErrorIndex)
        }

      case TokenType.Null =>
        reader.readNull()

      case actual => {
        reader.read()
        throw new IllegalStateException(s"Expected value token of type String, not $actual when reading OffsetTime value.  (Is your value wrapped in quotes?)\n" + reader.showError())
      }

    }

  override object serializer extends Serializer[OffsetTime] {

    override def serialize[J](tagged: TypeTagged[OffsetTime])(implicit ops: JsonOps[J]): SerializationResult[J] =
      tagged match {
        case TypeTagged(null) => SerializationSuccess(JsonNull())
        case TypeTagged(x)    => SerializationSuccess(JsonString(x.format(formatter)))
      }

  }

  override def write(value: OffsetTime, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value.format(formatter))
    }

}
