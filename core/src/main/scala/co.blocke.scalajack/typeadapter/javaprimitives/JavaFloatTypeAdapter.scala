package co.blocke.scalajack
package typeadapter
package javaprimitives

import co.blocke.scalajack.typeadapter.javaprimitives.BoxedFloatDeserializer.BoxedFloatType

import scala.reflect.runtime.universe.{ Type, typeOf }

object JavaFloatTypeAdapter extends TypeAdapterFactory.=:=[java.lang.Float] {

  override def create(next: TypeAdapterFactory)(implicit context: Context): TypeAdapter[java.lang.Float] =
    new JavaFloatTypeAdapter(context.typeAdapterOf[Float])

}

object BoxedFloatDeserializer {

  private val BoxedFloatType: Type = typeOf[java.lang.Float]

}

class BoxedFloatDeserializer(floatDeserializer: Deserializer[Float]) extends Deserializer[java.lang.Float] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[java.lang.Float] =
    json match {
      case JsonNull() =>
        DeserializationSuccess(TypeTagged(null, BoxedFloatType))

      case _ =>
        floatDeserializer.deserialize(path, json) map {
          case TypeTaggedFloat(floatValue) => TypeTagged(java.lang.Float.valueOf(floatValue), BoxedFloatType)
          case TypeTagged(floatValue)      => TypeTagged(java.lang.Float.valueOf(floatValue), BoxedFloatType)
        }
    }

}

class BoxedFloatSerializer(floatSerializer: Serializer[Float]) extends Serializer[java.lang.Float] {

  override def serialize[J](tagged: TypeTagged[java.lang.Float])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(null)  => SerializationSuccess(JsonNull())
      case TypeTagged(boxed) => floatSerializer.serialize(TypeTagged(boxed.floatValue))
    }

}

class JavaFloatTypeAdapter(primitiveTypeAdapter: TypeAdapter[Float]) extends TypeAdapter.=:=[java.lang.Float] {

  private val WrapperType: Type = typeOf[java.lang.Float]

  override object deserializer extends Deserializer[java.lang.Float] {

    override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[java.lang.Float] =
      json match {
        case JsonNull() =>
          DeserializationSuccess(TypeTagged(null, WrapperType))

        case _ =>
          primitiveTypeAdapter.deserializer.deserialize(path, json) map {
            case TypeTagged(primitive) =>
              val wrapper = java.lang.Float.valueOf(primitive)
              TypeTagged(wrapper, WrapperType)
          }
      }

  }

  override def read(reader: Reader): java.lang.Float =
    reader.peek match {
      case TokenType.Number =>
        java.lang.Float.valueOf(reader.readFloat())

      case TokenType.Null =>
        reader.readNull()

      case actual => {
        reader.read()
        throw new IllegalStateException(s"Expected value token of type Number, not $actual when reading Float value.  (Is your value wrapped in quotes?)\n" + reader.showError())
      }
    }

  override object serializer extends Serializer[java.lang.Float] {

    override def serialize[J](tagged: TypeTagged[java.lang.Float])(implicit ops: JsonOps[J]): SerializationResult[J] =
      tagged match {
        case TypeTagged(null) =>
          SerializationSuccess(JsonNull())

        case TypeTagged(wrapper) =>
          val primitive = wrapper.floatValue
          primitiveTypeAdapter.serializer.serialize(TypeTagged(primitive))
      }

  }

  override def write(value: java.lang.Float, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeFloat(value.floatValue)
    }

}
