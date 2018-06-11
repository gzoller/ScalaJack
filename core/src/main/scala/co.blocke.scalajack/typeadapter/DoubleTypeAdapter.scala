package co.blocke.scalajack
package typeadapter

class DoubleDeserializer extends Deserializer[Double] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[Double] =
    json match {
      case JsonDouble(doubleValue) => DeserializationSuccess(TypeTagged(doubleValue))
      // TODO handle other JSON types
      case _                       => DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON number"))
    }

}

class DoubleSerializer extends Serializer[Double] {

  override def serialize[J](tagged: TypeTagged[Double])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTaggedDouble(doubleValue) => SerializationSuccess(JsonDouble(doubleValue))
      case TypeTagged(doubleValue)       => SerializationSuccess(JsonDouble(doubleValue))
    }

}

object DoubleTypeAdapter extends TypeAdapter.=:=[Double] {

  override val deserializer: Deserializer[Double] = new DoubleDeserializer

  override val serializer: Serializer[Double] = new DoubleSerializer

  override def read(reader: Reader): Double =
    reader.readDouble()

  override def write(value: Double, writer: Writer): Unit =
    writer.writeDouble(value)

}
