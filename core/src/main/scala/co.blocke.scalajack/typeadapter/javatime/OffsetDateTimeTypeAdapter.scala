package co.blocke.scalajack
package typeadapter
package javatime

import java.time.OffsetDateTime
import java.time.format.{ DateTimeFormatter, DateTimeParseException }

import co.blocke.scalajack.typeadapter.javatime.OffsetDateTimeTypeAdapter.OffsetDateTimeType

import scala.reflect.runtime.universe.{ Type, typeOf }
import scala.util.{ Failure, Success, Try }

object OffsetDateTimeTypeAdapter extends OffsetDateTimeTypeAdapter(DateTimeFormatter.ISO_OFFSET_DATE_TIME) {

  val OffsetDateTimeType: Type = typeOf[OffsetDateTime]

}

class OffsetDateTimeDeserializer(formatter: DateTimeFormatter) extends Deserializer[OffsetDateTime] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[OffsetDateTime] =
    json match {
      case JsonString(x) =>
        DeserializationResult(path)(TypeTagged(OffsetDateTime.parse(x, formatter), OffsetDateTimeType), {
          case e: DateTimeParseException =>
            DeserializationError.Malformed(e)
        })

      case JsonNull() =>
        DeserializationSuccess(TypeTagged(null, OffsetDateTimeType))

      case _ =>
        DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON string"))
    }

}

class OffsetDateTimeSerializer(formatter: DateTimeFormatter) extends Serializer[OffsetDateTime] {

  override def serialize[J](tagged: TypeTagged[OffsetDateTime])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(JsonNull())
      case TypeTagged(x)    => SerializationSuccess(JsonString(x.format(formatter)))
    }

}

class OffsetDateTimeTypeAdapter(formatter: DateTimeFormatter) extends TypeAdapter.=:=[OffsetDateTime] with StringKind {

  override val deserializer: Deserializer[OffsetDateTime] = new OffsetDateTimeDeserializer(formatter)

  override val serializer: Serializer[OffsetDateTime] = new OffsetDateTimeSerializer(formatter)

  override def read(reader: Reader): OffsetDateTime =
    reader.peek match {
      case TokenType.String =>
        Try(OffsetDateTime.parse(reader.readString(), formatter)) match {
          case Success(u) => u
          case Failure(u) => throw new DateTimeParseException(u.getMessage + "\n" + reader.showError(), u.asInstanceOf[DateTimeParseException].getParsedString, u.asInstanceOf[DateTimeParseException].getErrorIndex)
        }

      case TokenType.Null =>
        reader.readNull()

      case actual =>
        reader.read()
        throw new IllegalStateException(s"Expected value token of type String, not $actual when reading OffsetDateTime value.  (Is your value wrapped in quotes?)\n" + reader.showError())
    }

  override def write(value: OffsetDateTime, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value.format(formatter))
    }

}
