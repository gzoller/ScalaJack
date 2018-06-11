package co.blocke.scalajack
package typeadapter
package javatime

import java.time.LocalDateTime
import java.time.format.{ DateTimeFormatter, DateTimeParseException }

import co.blocke.scalajack.typeadapter.javatime.LocalDateTimeDeserializer.LocalDateTimeType

import scala.reflect.runtime.universe.{ Type, typeOf }
import scala.util.{ Failure, Success, Try }

object LocalDateTimeTypeAdapter extends LocalDateTimeTypeAdapter(DateTimeFormatter.ISO_LOCAL_DATE_TIME)

object LocalDateTimeDeserializer {

  val LocalDateTimeType: Type = typeOf[LocalDateTime]

}

class LocalDateTimeDeserializer(formatter: DateTimeFormatter) extends Deserializer[LocalDateTime] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[LocalDateTime] =
    json match {
      case JsonString(x) =>
        DeserializationResult(path)(TypeTagged(LocalDateTime.parse(x, formatter), LocalDateTimeType), {
          case e: DateTimeParseException =>
            DeserializationError.Malformed(e)
        })

      case JsonNull() =>
        DeserializationSuccess(TypeTagged(null, LocalDateTimeType))

      case _ =>
        DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON string"))
    }

}

class LocalDateTimeSerializer(formatter: DateTimeFormatter) extends Serializer[LocalDateTime] {

  override def serialize[J](tagged: TypeTagged[LocalDateTime])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(JsonNull())
      case TypeTagged(x)    => SerializationSuccess(JsonString(x.format(formatter)))
    }

}

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
