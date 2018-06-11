package co.blocke.scalajack
package typeadapter
package javatime

import java.time.Instant
import java.time.format.DateTimeParseException

import co.blocke.scalajack.typeadapter.javatime.InstantDeserializer.InstantType

import scala.reflect.runtime.universe.{ Type, typeOf }
import scala.util.{ Failure, Success, Try }

object InstantDeserializer {

  val InstantType: Type = typeOf[Instant]

}

class InstantDeserializer extends Deserializer[Instant] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[Instant] =
    json match {
      case JsonString(x) =>
        DeserializationResult(path)(TypeTagged(Instant.parse(x), InstantType), {
          case e: DateTimeParseException =>
            DeserializationError.Malformed(e)
        })

      case JsonNull() =>
        DeserializationSuccess(TypeTagged(null, InstantType))

      case _ =>
        DeserializationFailure(path, DeserializationError.Malformed("Expected a JSON string"))
    }

}

class InstantSerializer extends Serializer[Instant] {

  override def serialize[J](tagged: TypeTagged[Instant])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(JsonNull())
      case TypeTagged(x)    => SerializationSuccess(JsonString(x.toString))
    }

}

object InstantTypeAdapter extends TypeAdapter.=:=[Instant] with StringKind {

  override val deserializer: Deserializer[Instant] = new InstantDeserializer

  override val serializer: Serializer[Instant] = new InstantSerializer

  override def read(reader: Reader): Instant =
    reader.peek match {
      case TokenType.String =>
        Try(Instant.parse(reader.readString())) match {
          case Success(u) => u
          case Failure(u) => throw new DateTimeParseException(u.getMessage + "\n" + reader.showError(), u.asInstanceOf[DateTimeParseException].getParsedString, u.asInstanceOf[DateTimeParseException].getErrorIndex)
        }

      case TokenType.Null =>
        reader.readNull()

      case actual => {
        reader.read()
        throw new IllegalStateException(s"Expected value token of type String, not $actual when reading Instant value.  (Is your value wrapped in quotes?)\n" + reader.showError())
      }
    }

  override def write(value: Instant, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value.toString)
    }

}
