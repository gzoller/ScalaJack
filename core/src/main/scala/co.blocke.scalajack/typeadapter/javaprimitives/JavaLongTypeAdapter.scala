package co.blocke.scalajack
package typeadapter
package javaprimitives

import scala.reflect.runtime.universe.{ Type, typeOf }

object JavaLongTypeAdapter extends TypeAdapterFactory.=:=[java.lang.Long] {

  override def create(next: TypeAdapterFactory)(implicit context: Context): TypeAdapter[java.lang.Long] =
    new JavaLongTypeAdapter(context.typeAdapterOf[Long])

}

class JavaLongTypeAdapter(primitiveTypeAdapter: TypeAdapter[Long]) extends TypeAdapter[java.lang.Long] {

  private val PrimitiveType: Type = typeOf[Long]
  private val WrapperType: Type = typeOf[java.lang.Long]

  override object deserializer extends Deserializer[java.lang.Long] {

    override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[java.lang.Long] =
      json match {
        case JsonNull() =>
          DeserializationSuccess(TypeTagged(null, WrapperType))

        case _ =>
          primitiveTypeAdapter.deserializer.deserialize(path, json) map {
            case TypeTagged(primitive) =>
              val wrapper = java.lang.Long.valueOf(primitive)
              TypeTagged(wrapper, WrapperType)
          }
      }

  }

  override def read(reader: Reader): java.lang.Long =
    reader.peek match {
      case TokenType.Number =>
        java.lang.Long.valueOf(reader.readLong())

      case TokenType.Null =>
        reader.readNull()

      case actual => {
        reader.read()
        throw new IllegalStateException(s"Expected value token of type Number, not $actual when reading Long value.  (Is your value wrapped in quotes?)\n" + reader.showError())
      }
    }

  override object serializer extends Serializer[java.lang.Long] {

    override def serialize[J](tagged: TypeTagged[java.lang.Long])(implicit ops: JsonOps[J]): SerializationResult[J] =
      tagged match {
        case TypeTagged(null) =>
          SerializationSuccess(JsonNull())

        case TypeTagged(wrapper) =>
          val primitive = wrapper.longValue
          primitiveTypeAdapter.serializer.serialize(TypeTagged(primitive, PrimitiveType))
      }

  }

  override def write(value: java.lang.Long, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeLong(value.longValue)
    }

}
