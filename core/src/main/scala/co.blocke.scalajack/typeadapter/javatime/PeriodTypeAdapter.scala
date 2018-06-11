package co.blocke.scalajack
package typeadapter
package javatime

import java.time.Period
import java.time.format.DateTimeParseException

import co.blocke.scalajack.typeadapter.javatime.PeriodDeserializer.PeriodType

import scala.reflect.runtime.universe.{ Type, typeOf }
import scala.util.{ Failure, Success, Try }

object PeriodDeserializer {

  val PeriodType: Type = typeOf[Period]

}

class PeriodDeserializer extends Deserializer[Period] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[Period] =
    json match {
      case JsonString(x) =>
        DeserializationResult(path)(TypeTagged(Period.parse(x), PeriodType), {
          case e: DateTimeParseException =>
            DeserializationError.Malformed(e)
        })

      case JsonNull() =>
        DeserializationSuccess(TypeTagged(null, PeriodType))

      case _ =>
        DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON string"))
    }

}

class PeriodSerializer extends Serializer[Period] {

  override def serialize[J](tagged: TypeTagged[Period])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(JsonNull())
      case TypeTagged(x)    => SerializationSuccess(JsonString(x.toString))
    }

}

object PeriodTypeAdapter extends TypeAdapter.=:=[Period] with StringKind {

  val PeriodType: Type = typeOf[Period]

  override val deserializer: Deserializer[Period] = new PeriodDeserializer

  override val serializer: Serializer[Period] = new PeriodSerializer

  override def read(reader: Reader): Period =
    reader.peek match {
      case TokenType.String =>
        Try(Period.parse(reader.readString())) match {
          case Success(u) => u
          case Failure(u) => throw new DateTimeParseException(u.getMessage + "\n" + reader.showError(), u.asInstanceOf[DateTimeParseException].getParsedString, u.asInstanceOf[DateTimeParseException].getErrorIndex)
        }

      case TokenType.Null =>
        reader.readNull()

      case actual => {
        reader.read()
        throw new IllegalStateException(s"Expected value token of type String, not $actual when reading Period value.  (Is your value wrapped in quotes?)\n" + reader.showError())
      }
    }

  override def write(value: Period, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value.toString)
    }

}
