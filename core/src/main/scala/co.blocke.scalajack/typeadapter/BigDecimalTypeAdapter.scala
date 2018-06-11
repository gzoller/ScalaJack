package co.blocke.scalajack
package typeadapter

import co.blocke.scalajack.typeadapter.BigDecimalDeserializer.BigDecimalType

import scala.reflect.runtime.universe.{ Type, typeOf }

object BigDecimalDeserializer {

  val BigDecimalType: Type = typeOf[BigDecimal]

}

class BigDecimalDeserializer extends Deserializer[BigDecimal] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[BigDecimal] =
    json match {
      case JsonInt(x)  => DeserializationResult(path)(TypeTagged(BigDecimal(x), BigDecimalType))
      case JsonLong(x) => DeserializationResult(path)(TypeTagged(BigDecimal(x), BigDecimalType))
      case _           => DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON number"))
    }

}

class BigDecimalSerializer extends Serializer[BigDecimal] {

  override def serialize[J](tagged: TypeTagged[BigDecimal])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(null)       => SerializationSuccess(JsonNull())
      case TypeTagged(bigDecimal) => SerializationSuccess(JsonDecimal(bigDecimal))
    }

}

object BigDecimalTypeAdapter extends TypeAdapter.=:=[BigDecimal] {

  override val deserializer: Deserializer[BigDecimal] = new BigDecimalDeserializer

  override val serializer: Serializer[BigDecimal] = new BigDecimalSerializer

  override def read(reader: Reader): BigDecimal =
    reader.peek match {
      case TokenType.Number =>
        reader.read(expected = TokenType.Number)
        BigDecimal(reader.tokenText)

      case TokenType.Null =>
        reader.readNull()

      case actual =>
        reader.read()
        throw new IllegalStateException(s"Expected value token of type Number, not $actual when reading BigDecimal value.  (Is your value wrapped in quotes?)\n" + reader.showError())
    }

  override def write(value: BigDecimal, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeRawValue(value.toString)
    }

}
