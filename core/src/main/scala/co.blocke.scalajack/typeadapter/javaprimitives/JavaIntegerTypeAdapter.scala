package co.blocke.scalajack
package typeadapter
package javaprimitives

import scala.reflect.runtime.universe.{ Type, typeOf }

object JavaIntegerTypeAdapter extends TypeAdapterFactory.=:=[java.lang.Integer] {

  override def create(next: TypeAdapterFactory)(implicit context: Context): TypeAdapter[java.lang.Integer] =
    new JavaIntegerTypeAdapter(context.typeAdapterOf[Int])

}

class JavaIntegerTypeAdapter(primitiveTypeAdapter: TypeAdapter[Int]) extends TypeAdapter[java.lang.Integer] {

  private val PrimitiveType: Type = typeOf[Int]
  private val WrapperType: Type = typeOf[java.lang.Integer]

  override object deserializer extends Deserializer[java.lang.Integer] {

    override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[java.lang.Integer] =
      json match {
        case JsonNull() =>
          DeserializationSuccess(TypeTagged(null, WrapperType))

        case _ =>
          primitiveTypeAdapter.deserializer.deserialize(path, json) map {
            case TypeTagged(primitive) =>
              val wrapper = java.lang.Integer.valueOf(primitive)
              TypeTagged(wrapper, WrapperType)
          }
      }

  }

  override def read(reader: Reader): java.lang.Integer =
    reader.peek match {
      case TokenType.Number =>
        java.lang.Integer.valueOf(reader.readInt())

      case TokenType.Null =>
        reader.readNull()

      case actual => {
        reader.read()
        throw new IllegalStateException(s"Expected value token of type Number, not $actual when reading Integer value.  (Is your value wrapped in quotes?)\n" + reader.showError())
      }
    }

  override object serializer extends Serializer[java.lang.Integer] {

    override def serialize[J](tagged: TypeTagged[java.lang.Integer])(implicit ops: JsonOps[J]): SerializationResult[J] =
      tagged match {
        case TypeTagged(null) =>
          SerializationSuccess(JsonNull())

        case TypeTagged(wrapper) =>
          val primitive = wrapper.intValue
          primitiveTypeAdapter.serializer.serialize(TypeTagged(primitive, PrimitiveType))
      }

  }

  override def write(value: java.lang.Integer, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeInt(value.intValue)
    }

}
