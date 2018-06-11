package co.blocke.scalajack
package typeadapter
package javatime

import java.time.LocalDate
import java.time.format.{ DateTimeFormatter, DateTimeParseException }

import co.blocke.scalajack.typeadapter.javatime.LocalDateTypeAdapter.LocalDateType

import scala.reflect.runtime.universe.{ Type, typeOf }
import scala.util.{ Failure, Success, Try }

object LocalDateTypeAdapter extends LocalDateTypeAdapter(DateTimeFormatter.ISO_LOCAL_DATE) {

  val LocalDateType: Type = typeOf[LocalDate]

}

class LocalDateDeserializer(formatter: DateTimeFormatter) extends Deserializer[LocalDate] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[LocalDate] =
    json match {
      case JsonString(x) =>
        DeserializationResult(path)(TypeTagged(LocalDate.parse(x, formatter), LocalDateType), {
          case e: DateTimeParseException =>
            DeserializationError.Malformed(e)
        })

      case JsonNull() =>
        DeserializationSuccess(TypeTagged(null, LocalDateType))

      case _ =>
        DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON string"))
    }

}

class LocalDateSerializer(formatter: DateTimeFormatter) extends Serializer[LocalDate] {

  override def serialize[J](tagged: TypeTagged[LocalDate])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(JsonNull())
      case TypeTagged(x)    => SerializationSuccess(JsonString(x.format(formatter)))
    }

}

class LocalDateTypeAdapter(formatter: DateTimeFormatter) extends TypeAdapter.=:=[LocalDate] with StringKind {

  override val deserializer: Deserializer[LocalDate] = new LocalDateDeserializer(formatter)

  override val serializer: Serializer[LocalDate] = new LocalDateSerializer(formatter)

  override def read(reader: Reader): LocalDate =
    reader.peek match {
      case TokenType.String =>
        Try(LocalDate.parse(reader.readString())) match {
          case Success(u) => u
          case Failure(u) => throw new DateTimeParseException(u.getMessage + "\n" + reader.showError(), u.asInstanceOf[DateTimeParseException].getParsedString, u.asInstanceOf[DateTimeParseException].getErrorIndex)
        }

      case TokenType.Null =>
        reader.readNull()

      case actual => {
        reader.read()
        throw new IllegalStateException(s"Expected value token of type String, not $actual when reading LocalDate value.  (Is your value wrapped in quotes?)\n" + reader.showError())
      }
    }

  override def write(value: LocalDate, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value.format(formatter))
    }

}
