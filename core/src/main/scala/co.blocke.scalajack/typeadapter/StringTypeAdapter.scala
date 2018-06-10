package co.blocke.scalajack
package typeadapter

import scala.reflect.runtime.universe.{ Type, typeOf }

object StringTypeAdapter extends TypeAdapter.=:=[String] with StringKind {

  override object deserializer extends Deserializer[String] {

    private val StringType: Type = typeOf[String]

    override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[String] =
      json match {
        case JsonString(value) => DeserializationSuccess(TypeTagged[String](value, StringType))
        case _                 => DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON string"))
      }

  }

  override def read(reader: Reader): String = {
    reader.peek match {
      case TokenType.String =>
        reader.readString()

      case TokenType.Null =>
        reader.readNull()

      case actual =>
        reader.skipValue()
        throw new IllegalStateException(s"Expected value token of type String, not $actual when reading String value.\n" + reader.showError())
    }
  }

  override def write(value: String, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value)
    }

}
