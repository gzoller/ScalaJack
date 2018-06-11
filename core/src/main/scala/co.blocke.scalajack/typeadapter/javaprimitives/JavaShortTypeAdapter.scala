package co.blocke.scalajack
package typeadapter
package javaprimitives

import co.blocke.scalajack.typeadapter.javaprimitives.BoxedShortDeserializer.BoxedShortType

import scala.reflect.runtime.universe.{ Type, typeOf }

object JavaShortTypeAdapter extends TypeAdapterFactory.=:=[java.lang.Short] {

  override def create(next: TypeAdapterFactory)(implicit context: Context): TypeAdapter[java.lang.Short] =
    new JavaShortTypeAdapter(context.typeAdapterOf[Short])

}

object BoxedShortDeserializer {

  private val BoxedShortType: Type = typeOf[java.lang.Short]

}

class BoxedShortDeserializer(shortDeserializer: Deserializer[Short]) extends Deserializer[java.lang.Short] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[java.lang.Short] =
    json match {
      case JsonNull() =>
        DeserializationSuccess(TypeTagged(null, BoxedShortType))

      case _ =>
        shortDeserializer.deserialize(path, json) map {
          case TypeTaggedShort(shortValue) => TypeTagged(java.lang.Short.valueOf(shortValue), BoxedShortType)
          case TypeTagged(shortValue)      => TypeTagged(java.lang.Short.valueOf(shortValue), BoxedShortType)
        }
    }

}

class BoxedShortSerializer(shortSerializer: Serializer[Short]) extends Serializer[java.lang.Short] {

  override def serialize[J](tagged: TypeTagged[java.lang.Short])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(null)  => SerializationSuccess(JsonNull())
      case TypeTagged(boxed) => shortSerializer.serialize(TypeTagged(boxed.shortValue))
    }

}

class JavaShortTypeAdapter(primitiveTypeAdapter: TypeAdapter[Short]) extends TypeAdapter[java.lang.Short] {

  override object deserializer extends Deserializer[java.lang.Short] {

    private val BoxedShortType: Type = typeOf[java.lang.Short]

    override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[java.lang.Short] =
      json match {
        case JsonNull() =>
          DeserializationSuccess(TypeTagged(null, BoxedShortType))

        case _ =>
          primitiveTypeAdapter.deserializer.deserialize(path, json) map {
            case TypeTaggedShort(shortValue) => TypeTagged(java.lang.Short.valueOf(shortValue), BoxedShortType)
            case TypeTagged(shortValue)      => TypeTagged(java.lang.Short.valueOf(shortValue), BoxedShortType)
          }
      }

  }

  override def read(reader: Reader): java.lang.Short =
    reader.peek match {
      case TokenType.Number =>
        java.lang.Short.valueOf(reader.readShort())

      case TokenType.Null =>
        reader.readNull()

      case actual => {
        reader.read()
        throw new IllegalStateException(s"Expected value token of type Number, not $actual when reading Short value.  (Is your value wrapped in quotes?)\n" + reader.showError())
      }
    }

  override object serializer extends Serializer[java.lang.Short] {

    override def serialize[J](tagged: TypeTagged[java.lang.Short])(implicit ops: JsonOps[J]): SerializationResult[J] =
      tagged match {
        case TypeTagged(null) =>
          SerializationSuccess(JsonNull())

        case TypeTagged(wrapper) =>
          val primitive = wrapper.shortValue
          primitiveTypeAdapter.serializer.serialize(TypeTagged(primitive))
      }

  }

  override def write(value: java.lang.Short, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeShort(value.shortValue)
    }

}
