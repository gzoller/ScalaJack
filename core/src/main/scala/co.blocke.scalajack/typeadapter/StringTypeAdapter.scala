package co.blocke.scalajack
package typeadapter

import co.blocke.scalajack.typeadapter.StringDeserializer.StringType

import scala.reflect.runtime.universe.{ Type, typeOf }

object StringDeserializer {

  val StringType: Type = typeOf[String]

}

class StringDeserializer extends Deserializer[String] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[String] =
    json match {
      case JsonNull()        => DeserializationSuccess(TypeTagged(null, StringType))
      case JsonString(value) => DeserializationSuccess(TypeTagged(value, StringType))
      case _                 => DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON string"))
    }

}

class StringSerializer extends Serializer[String] {

  override def serialize[J](tagged: TypeTagged[String])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(null)  => SerializationSuccess(JsonNull())
      case TypeTagged(value) => SerializationSuccess(JsonString(value))
    }

}

object StringTypeAdapter extends TypeAdapter.=:=[String] with StringKind {

  override val deserializer: Deserializer[String] = new StringDeserializer

  override val serializer: Serializer[String] = new StringSerializer

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
