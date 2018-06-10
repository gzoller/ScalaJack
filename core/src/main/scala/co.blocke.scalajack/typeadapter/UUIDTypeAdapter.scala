package co.blocke.scalajack
package typeadapter

import java.util.UUID

import scala.reflect.runtime.universe.{ Type, typeOf }
import scala.util.{ Failure, Success, Try }

object UUIDTypeAdapter extends TypeAdapter.=:=[UUID] {

  override object deserializer extends Deserializer[UUID] {

    private val UUIDType: Type = typeOf[UUID]

    override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[UUID] =
      json match {
        case JsonString(x) => DeserializationResult(path)(TypeTagged(UUID.fromString(x), UUIDType))
        case _             => DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON string"))
      }

  }

  override def read(reader: Reader): UUID =
    reader.peek match {
      case TokenType.String =>
        Try(UUID.fromString(reader.readString())) match {
          case Success(u) => u
          case Failure(u) => throw new java.lang.IllegalArgumentException(u.getMessage + "\n" + reader.showError())
        }

      case TokenType.Null =>
        reader.readNull()

      case actual => {
        reader.read()
        throw new IllegalStateException(s"Expected value token of type String, not $actual when reading UUID value.\n" + reader.showError())
      }
    }

  override def write(value: UUID, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value.toString)
    }

}
