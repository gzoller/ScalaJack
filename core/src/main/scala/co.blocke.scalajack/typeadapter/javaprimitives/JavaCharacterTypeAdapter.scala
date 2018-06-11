package co.blocke.scalajack
package typeadapter
package javaprimitives

import co.blocke.scalajack.typeadapter.javaprimitives.BoxedCharDeserializer.BoxedCharType

import scala.reflect.runtime.universe.{ Type, typeOf }

object JavaCharacterTypeAdapter extends TypeAdapterFactory.=:=[java.lang.Character] {

  override def create(next: TypeAdapterFactory)(implicit context: Context): TypeAdapter[java.lang.Character] =
    new JavaCharacterTypeAdapter(context.typeAdapterOf[Char])

}

object BoxedCharDeserializer {

  private val BoxedCharType: Type = typeOf[java.lang.Character]

}

class BoxedCharDeserializer(charDeserializer: Deserializer[Char]) extends Deserializer[java.lang.Character] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[java.lang.Character] =
    json match {
      case JsonNull() =>
        DeserializationSuccess(TypeTagged(null, BoxedCharType))

      case _ =>
        charDeserializer.deserialize(path, json) map {
          case TypeTaggedChar(charValue) => TypeTagged(java.lang.Character.valueOf(charValue), BoxedCharType)
          case TypeTagged(charValue)     => TypeTagged(java.lang.Character.valueOf(charValue), BoxedCharType)
        }
    }

}

class BoxedCharSerializer(charSerializer: Serializer[Char]) extends Serializer[java.lang.Character] {

  override def serialize[J](tagged: TypeTagged[Character])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(null)  => SerializationSuccess(JsonNull())
      case TypeTagged(boxed) => charSerializer.serialize(TypeTagged(boxed.charValue))
    }

}

class JavaCharacterTypeAdapter(primitiveTypeAdapter: TypeAdapter[Char]) extends TypeAdapter.=:=[java.lang.Character] with StringKind {

  private val WrapperType: Type = typeOf[java.lang.Character]

  override object deserializer extends Deserializer[java.lang.Character] {

    override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[java.lang.Character] =
      json match {
        case JsonNull() =>
          DeserializationSuccess(TypeTagged(null, WrapperType))

        case _ =>
          primitiveTypeAdapter.deserializer.deserialize(path, json) map {
            case TypeTagged(primitive) =>
              val wrapper = java.lang.Character.valueOf(primitive)
              TypeTagged(wrapper, WrapperType)
          }
      }

  }

  override def read(reader: Reader): java.lang.Character =
    reader.peek match {
      case TokenType.String =>
        java.lang.Character.valueOf(reader.readString().head)

      case TokenType.Null =>
        reader.readNull()

      case actual => {
        reader.read()
        throw new IllegalStateException(s"Expected value token of type String, not $actual when reading Character value.  (Is your value wrapped in quotes?)\n" + reader.showError())
      }
    }

  override object serializer extends Serializer[java.lang.Character] {

    override def serialize[J](tagged: TypeTagged[Character])(implicit ops: JsonOps[J]): SerializationResult[J] =
      tagged match {
        case TypeTagged(null) =>
          SerializationSuccess(JsonNull())

        case TypeTagged(wrapper) =>
          val primitive = wrapper.charValue
          primitiveTypeAdapter.serializer.serialize(TypeTagged(primitive))
      }

  }

  override def write(value: java.lang.Character, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeChar(value.charValue)
    }

}
