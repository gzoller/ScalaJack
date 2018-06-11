package co.blocke.scalajack
package typeadapter
package javaprimitives

import co.blocke.scalajack.typeadapter.javaprimitives.BoxedLongDeserializer.BoxedLongType

import scala.reflect.runtime.universe.{ Type, typeOf }

object JavaLongTypeAdapter extends TypeAdapterFactory.=:=[java.lang.Long] {

  override def create(next: TypeAdapterFactory)(implicit context: Context): TypeAdapter[java.lang.Long] = {
    val longTypeAdapter = context.typeAdapterOf[Long]
    new JavaLongTypeAdapter(
      deserializer = new BoxedLongDeserializer(longTypeAdapter.deserializer),
      serializer   = new BoxedLongSerializer(longTypeAdapter.serializer))
  }

}

object BoxedLongDeserializer {

  private val BoxedLongType: Type = typeOf[java.lang.Long]

}

class BoxedLongDeserializer(longDeserializer: Deserializer[Long]) extends Deserializer[java.lang.Long] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[java.lang.Long] =
    json match {
      case JsonNull() =>
        DeserializationSuccess(TypeTagged(null, BoxedLongType))

      case _ =>
        longDeserializer.deserialize(path, json) map {
          case TypeTaggedLong(longValue) => TypeTagged(java.lang.Long.valueOf(longValue), BoxedLongType)
          case TypeTagged(longValue)     => TypeTagged(java.lang.Long.valueOf(longValue), BoxedLongType)
        }
    }

}

class BoxedLongSerializer(longSerializer: Serializer[Long]) extends Serializer[java.lang.Long] {

  override def serialize[J](tagged: TypeTagged[java.lang.Long])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(null)  => SerializationSuccess(JsonNull())
      case TypeTagged(boxed) => longSerializer.serialize(TypeTagged(boxed.longValue))
    }

}

class JavaLongTypeAdapter(override val deserializer: Deserializer[java.lang.Long], override val serializer: Serializer[java.lang.Long]) extends TypeAdapter[java.lang.Long] {

  override def read(reader: Reader): java.lang.Long =
    reader.peek match {
      case TokenType.Number =>
        java.lang.Long.valueOf(reader.readLong())

      case TokenType.Null =>
        reader.readNull()

      case actual =>
        reader.read()
        throw new IllegalStateException(s"Expected value token of type Number, not $actual when reading Long value.  (Is your value wrapped in quotes?)\n" + reader.showError())
    }

  override def write(value: java.lang.Long, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeLong(value.longValue)
    }

}
