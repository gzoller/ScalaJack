package co.blocke.scalajack
package typeadapter

import scala.reflect.runtime.universe.{ Type, typeOf }

object BooleanTypeAdapter extends TypeAdapter.=:=[Boolean] {

  override object deserializer extends Deserializer[Boolean] {

    private val BooleanType: Type = typeOf[Boolean]

    override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[Boolean] =
      json match {
        case JsonBoolean(value) => DeserializationSuccess(TypeTagged[Boolean](value, BooleanType))
        case _                  => DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON boolean"))
      }

  }

  override def read(reader: Reader): Boolean = {
    reader.peek match {
      case TokenType.False =>
        reader.read(expected = TokenType.False)
        false

      case TokenType.True =>
        reader.read(expected = TokenType.True)
        true

      case TokenType.Null =>
        throw new IllegalStateException("Expected token of type Boolean, not Null\n" + reader.showError())

      case actual => {
        reader.read()
        throw new IllegalStateException(s"Expected value token of type True or False, not $actual when reading Boolean value.  (Is your value wrapped in quotes or a number?)\n" + reader.showError())
      }
    }
  }

  override object serializer extends Serializer[Boolean] {

    override def serialize[J](tagged: TypeTagged[Boolean])(implicit ops: JsonOps[J]): SerializationResult[J] =
      tagged match {
        case TypeTagged(value) => SerializationSuccess(JsonBoolean(value))
      }

  }

  override def write(value: Boolean, writer: Writer): Unit =
    if (value) {
      writer.writeTrue()
    } else {
      writer.writeFalse()
    }

}
