package co.blocke.scalajack
package typeadapter
package javaprimitives

import co.blocke.scalajack.typeadapter.javaprimitives.BoxedByteDeserializer.BoxedByteType

import scala.reflect.runtime.universe.{ Type, typeOf }

object JavaByteTypeAdapter extends TypeAdapterFactory.=:=[java.lang.Byte] {

  override def create(next: TypeAdapterFactory)(implicit context: Context): TypeAdapter[java.lang.Byte] =
    new JavaByteTypeAdapter(context.typeAdapterOf[Byte])

}

object BoxedByteDeserializer {

  private val BoxedByteType: Type = typeOf[java.lang.Byte]

}

class BoxedByteDeserializer(byteDeserializer: Deserializer[Byte]) extends Deserializer[java.lang.Byte] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[java.lang.Byte] =
    json match {
      case JsonNull() =>
        DeserializationSuccess(TypeTagged(null, BoxedByteType))

      case _ =>
        byteDeserializer.deserialize(path, json) map {
          case TypeTaggedByte(byteValue) => TypeTagged(java.lang.Byte.valueOf(byteValue), BoxedByteType)
          case TypeTagged(byteValue)     => TypeTagged(java.lang.Byte.valueOf(byteValue), BoxedByteType)
        }
    }

}

class BoxedByteSerializer(byteSerializer: Serializer[Byte]) extends Serializer[java.lang.Byte] {

  override def serialize[J](tagged: TypeTagged[java.lang.Byte])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(null)  => SerializationSuccess(JsonNull())
      case TypeTagged(boxed) => byteSerializer.serialize(TypeTagged(boxed.byteValue))
    }

}

class JavaByteTypeAdapter(primitiveTypeAdapter: TypeAdapter[Byte]) extends TypeAdapter.=:=[java.lang.Byte] {

  private val WrapperType: Type = typeOf[java.lang.Byte]

  override object deserializer extends Deserializer[java.lang.Byte] {

    override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[java.lang.Byte] =
      json match {
        case JsonNull() =>
          DeserializationSuccess(TypeTagged(null, WrapperType))

        case _ =>
          primitiveTypeAdapter.deserializer.deserialize(path, json) map {
            case TypeTagged(primitive) =>
              val wrapper = java.lang.Byte.valueOf(primitive)
              TypeTagged(wrapper, WrapperType)
          }
      }

  }

  override def read(reader: Reader): java.lang.Byte =
    reader.peek match {
      case TokenType.Null =>
        reader.readNull()

      case TokenType.Number =>
        java.lang.Byte.valueOf(reader.readByte())

      case actual => {
        reader.read()
        throw new IllegalStateException(s"Expected value token of type Number, not $actual when reading Byte value.  (Is your value wrapped in quotes?)\n" + reader.showError())
      }
    }

  override object serializer extends Serializer[java.lang.Byte] {

    override def serialize[J](tagged: TypeTagged[java.lang.Byte])(implicit ops: JsonOps[J]): SerializationResult[J] =
      tagged match {
        case TypeTagged(null) => SerializationSuccess(JsonNull())
        case TypeTagged(wrapper) =>
          val primitive = wrapper.byteValue
          primitiveTypeAdapter.serializer.serialize(TypeTagged(primitive))
      }

  }

  override def write(value: java.lang.Byte, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeByte(value.byteValue)
    }

}
