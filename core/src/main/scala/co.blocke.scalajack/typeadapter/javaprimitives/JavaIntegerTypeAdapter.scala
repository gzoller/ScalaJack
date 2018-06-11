package co.blocke.scalajack
package typeadapter
package javaprimitives

import co.blocke.scalajack.typeadapter.javaprimitives.BoxedIntDeserializer.BoxedIntType

import scala.reflect.runtime.universe.{ Type, typeOf }

object JavaIntegerTypeAdapter extends TypeAdapterFactory.=:=[java.lang.Integer] {

  override def create(next: TypeAdapterFactory)(implicit context: Context): TypeAdapter[java.lang.Integer] = {
    val intTypeAdapter = context.typeAdapterOf[Int]
    new JavaIntegerTypeAdapter(
      deserializer = new BoxedIntDeserializer(intTypeAdapter.deserializer),
      serializer   = new BoxedIntSerializer(intTypeAdapter.serializer))
  }

}

object BoxedIntDeserializer {

  private val BoxedIntType: Type = typeOf[java.lang.Integer]

}

class BoxedIntDeserializer(intDeserializer: Deserializer[Int]) extends Deserializer[java.lang.Integer] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[java.lang.Integer] =
    json match {
      case JsonNull() =>
        DeserializationSuccess(TypeTagged(null, BoxedIntType))

      case _ =>
        intDeserializer.deserialize(path, json) map {
          case TypeTaggedInt(intValue) => TypeTagged(java.lang.Integer.valueOf(intValue), BoxedIntType)
          case TypeTagged(intValue)    => TypeTagged(java.lang.Integer.valueOf(intValue), BoxedIntType)
        }
    }

}

class BoxedIntSerializer(intSerializer: Serializer[Int]) extends Serializer[java.lang.Integer] {

  override def serialize[J](tagged: TypeTagged[java.lang.Integer])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(null)  => SerializationSuccess(JsonNull())
      case TypeTagged(boxed) => intSerializer.serialize(TypeTagged(boxed.intValue))
    }

}

class JavaIntegerTypeAdapter(override val deserializer: Deserializer[java.lang.Integer], override val serializer: Serializer[java.lang.Integer]) extends TypeAdapter[java.lang.Integer] {

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

  override def write(value: java.lang.Integer, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeInt(value.intValue)
    }

}
