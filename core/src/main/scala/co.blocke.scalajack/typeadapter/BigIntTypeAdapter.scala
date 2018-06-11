package co.blocke.scalajack
package typeadapter

import co.blocke.scalajack.typeadapter.BigIntDeserializer.BigIntType

import scala.reflect.runtime.universe.{ Type, typeOf }

object BigIntDeserializer {

  val BigIntType: Type = typeOf[BigInt]

}

class BigIntDeserializer extends Deserializer[BigInt] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[BigInt] =
    json match {
      case JsonNull()           => DeserializationSuccess(TypeTagged(null, BigIntType))
      case JsonLong(longValue)  => DeserializationSuccess(TypeTagged(BigInt(longValue), BigIntType))
      case JsonInt(scalaBigInt) => DeserializationSuccess(TypeTagged(scalaBigInt, BigIntType))

      case JsonDecimal(scalaBigDecimal) =>
        DeserializationResult(path)(TypeTagged(BigInt(scalaBigDecimal.bigDecimal.toBigIntegerExact), BigIntType), {
          case e: ArithmeticException =>
            DeserializationError.Malformed(e)
        })

      case JsonDouble(doubleValue) =>
        DeserializationResult(path)(TypeTagged(BigInt(new java.math.BigDecimal(doubleValue).toBigIntegerExact), BigIntType), {
          case e: ArithmeticException =>
            DeserializationError.Malformed(e)
        })

      case _ => DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON number"))
    }

}

class BigIntSerializer extends Serializer[BigInt] {

  override def serialize[J](tagged: TypeTagged[BigInt])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(null)   => SerializationSuccess(JsonNull())
      case TypeTagged(bigInt) => SerializationSuccess(JsonInt(bigInt))
    }

}

object BigIntTypeAdapter extends TypeAdapter.=:=[BigInt] {

  override val deserializer: Deserializer[BigInt] = new BigIntDeserializer

  override val serializer: Serializer[BigInt] = new BigIntSerializer

  override def read(reader: Reader): BigInt =
    reader.peek match {
      case TokenType.Number =>
        reader.read(expected = TokenType.Number)
        BigInt(reader.tokenText)

      case TokenType.Null =>
        reader.readNull()

      case actual =>
        reader.read()
        throw new IllegalStateException(s"Expected value token of type Number, not $actual when reading BigInt value.  (Is your value wrapped in quotes?)\n" + reader.showError())
    }

  override def write(value: BigInt, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeRawValue(value.toString)
    }

}
