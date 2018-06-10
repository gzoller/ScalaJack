package co.blocke.scalajack
package typeadapter
package javaprimitives

import scala.reflect.runtime.universe.{ Type, typeOf }

object JavaDoubleTypeAdapter extends TypeAdapterFactory.=:=[java.lang.Double] {

  override def create(next: TypeAdapterFactory)(implicit context: Context): TypeAdapter[java.lang.Double] =
    new JavaDoubleTypeAdapter(context.typeAdapterOf[Double])

}

class JavaDoubleTypeAdapter(primitiveTypeAdapter: TypeAdapter[Double]) extends TypeAdapter.=:=[java.lang.Double] {

  private val PrimitiveType: Type = typeOf[Double]
  private val WrapperType: Type = typeOf[java.lang.Double]

  override object deserializer extends Deserializer[java.lang.Double] {

    override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[java.lang.Double] =
      json match {
        case JsonNull() =>
          DeserializationSuccess(TypeTagged(null, WrapperType))

        case _ =>
          primitiveTypeAdapter.deserializer.deserialize(path, json) map {
            case TypeTagged(primitive) =>
              val wrapper = java.lang.Double.valueOf(primitive)
              TypeTagged(wrapper, WrapperType)
          }
      }

  }

  override def read(reader: Reader): java.lang.Double =
    reader.peek match {
      case TokenType.Number =>
        java.lang.Double.valueOf(reader.readDouble())

      case TokenType.Null =>
        reader.readNull()

      case actual => {
        reader.read()
        throw new IllegalStateException(s"Expected value token of type Number, not $actual when reading Double value.  (Is your value wrapped in quotes?)\n" + reader.showError())
      }
    }

  override object serializer extends Serializer[java.lang.Double] {

    override def serialize[J](tagged: TypeTagged[java.lang.Double])(implicit ops: JsonOps[J]): SerializationResult[J] =
      tagged match {
        case TypeTagged(null) =>
          SerializationSuccess(JsonNull())

        case TypeTagged(wrapper) =>
          val primitive = wrapper.doubleValue
          primitiveTypeAdapter.serializer.serialize(TypeTagged(primitive, PrimitiveType))
      }

  }

  override def write(value: java.lang.Double, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeDouble(value.doubleValue)
    }

}
