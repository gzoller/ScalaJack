package co.blocke.scalajack
package typeadapter
package javaprimitives

import scala.reflect.runtime.universe.{ Type, typeOf }

object JavaBooleanTypeAdapter extends TypeAdapterFactory.=:=[java.lang.Boolean] {

  override def create(next: TypeAdapterFactory)(implicit context: Context): TypeAdapter[java.lang.Boolean] =
    new JavaBooleanTypeAdapter(context.typeAdapterOf[Boolean])

}

class JavaBooleanTypeAdapter(next: TypeAdapter[Boolean]) extends TypeAdapter[java.lang.Boolean] {

  override object deserializer extends Deserializer[java.lang.Boolean] {

    private val BooleanType: Type = typeOf[java.lang.Boolean]

    override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[java.lang.Boolean] =
      json match {
        case JsonNull() =>
          DeserializationSuccess(TypeTagged(null, BooleanType))

        case _ =>
          next.deserializer.deserialize(path, json) map {
            case TypeTagged(x) => TypeTagged(java.lang.Boolean.valueOf(x), BooleanType)
          }
      }

  }

  override def read(reader: Reader): java.lang.Boolean =
    reader.peek match {
      case TokenType.False | TokenType.True =>
        java.lang.Boolean.valueOf(reader.readBoolean())

      case TokenType.Null =>
        reader.readNull()

      case actual => {
        reader.read()
        throw new IllegalStateException(s"Expected value token of type True or False, not $actual when reading Boolean value.  (Is your value wrapped in quotes?)\n" + reader.showError())
      }
    }

  override object serializer extends Serializer[java.lang.Boolean] {

    override def serialize[J](tagged: TypeTagged[java.lang.Boolean])(implicit ops: JsonOps[J]): SerializationResult[J] =
      tagged match {
        case TypeTagged(null) => SerializationSuccess(JsonNull())
        case TypeTagged(x)    => SerializationSuccess(JsonBoolean(x.booleanValue))
      }

  }

  override def write(value: java.lang.Boolean, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeBoolean(value.booleanValue)
    }

}
