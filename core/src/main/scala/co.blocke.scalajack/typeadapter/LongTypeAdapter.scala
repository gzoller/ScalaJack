package co.blocke.scalajack
package typeadapter

import scala.reflect.runtime.universe.{ Type, typeOf }

object LongTypeAdapter extends TypeAdapter.=:=[Long] {

  override object deserializer extends Deserializer[Long] {

    private val LongType: Type = typeOf[Long]

    override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[Long] =
      json match {
        case JsonLong(x) => DeserializationSuccess(TypeTagged(x, LongType))
        case _           => DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON number"))
      }

  }

  override def read(reader: Reader): Long =
    reader.readLong()

  override object serializer extends Serializer[Long] {

    override def serialize[J](tagged: TypeTagged[Long])(implicit ops: JsonOps[J]): SerializationResult[J] =
      tagged match {
        case TypeTagged(x) => SerializationSuccess(JsonLong(x))
      }

  }

  override def write(value: Long, writer: Writer): Unit =
    writer.writeLong(value)

}
