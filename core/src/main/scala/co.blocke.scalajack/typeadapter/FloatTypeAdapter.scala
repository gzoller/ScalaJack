package co.blocke.scalajack
package typeadapter

import co.blocke.scalajack.NumberConverters._

class FloatDeserializer extends Deserializer[Float] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[Float] =
    json match {
      case JsonDecimal(bigDecimal) =>
        ??? // TODO

      case JsonDouble(doubleValue) =>
        DeserializationResult(path)(TypeTagged(doubleValue.toFloatExact), {
          case e: ArithmeticException =>
            DeserializationError.Malformed(e)
        })

      case JsonLong(longValue) =>
        DeserializationResult(path)(TypeTagged(longValue.toFloatExact), {
          case e: ArithmeticException =>
            DeserializationError.Malformed(e)
        })

      case _ =>
        DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON number"))
    }

}

class FloatSerializer extends Serializer[Float] {

  override def serialize[J](tagged: TypeTagged[Float])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTaggedFloat(floatValue) => SerializationSuccess(JsonDouble(floatValue.doubleValue))
    }

}

object FloatTypeAdapter extends TypeAdapter.=:=[Float] {

  override val deserializer: Deserializer[Float] = new FloatDeserializer

  override val serializer: Serializer[Float] = new FloatSerializer

  override def read(reader: Reader): Float =
    reader.readFloat()

  override def write(value: Float, writer: Writer): Unit =
    writer.writeFloat(value)

}
