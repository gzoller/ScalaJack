package co.blocke.scalajack
package typeadapter
package javaprimitives

import co.blocke.scalajack.typeadapter.javaprimitives.BoxedBooleanDeserializer.BoxedBooleanType

import scala.reflect.runtime.universe.{ Type, TypeTag, typeOf }

object JavaBooleanTypeAdapter extends TypeAdapterFactory.=:=[java.lang.Boolean] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[java.lang.Boolean]): TypeAdapter[java.lang.Boolean] = {
    val booleanTypeAdapter = context.typeAdapterOf[Boolean]
    new JavaBooleanTypeAdapter(
      deserializer = new BoxedBooleanDeserializer(booleanTypeAdapter.deserializer),
      serializer   = new BoxedBooleanSerializer(booleanTypeAdapter.serializer))
  }

}

object BoxedBooleanDeserializer {

  val BoxedBooleanType: Type = typeOf[java.lang.Boolean]

}

class BoxedBooleanDeserializer(booleanDeserializer: Deserializer[Boolean]) extends Deserializer[java.lang.Boolean] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[java.lang.Boolean] =
    json match {
      case JsonNull() =>
        DeserializationSuccess(TypeTagged(null, BoxedBooleanType))

      case _ =>
        booleanDeserializer.deserialize(path, json) map {
          case TypeTaggedBoolean(booleanValue) => TypeTagged(java.lang.Boolean.valueOf(booleanValue), BoxedBooleanType)
          case TypeTagged(booleanValue)        => TypeTagged(java.lang.Boolean.valueOf(booleanValue), BoxedBooleanType)
        }
    }

}

class BoxedBooleanSerializer(booleanSerializer: Serializer[Boolean]) extends Serializer[java.lang.Boolean] {

  override def serialize[J](tagged: TypeTagged[java.lang.Boolean])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(null)  => SerializationSuccess(JsonNull())
      case TypeTagged(boxed) => booleanSerializer.serialize(TypeTagged(boxed.booleanValue))
    }

}

class JavaBooleanTypeAdapter(override val deserializer: Deserializer[java.lang.Boolean], override val serializer: Serializer[java.lang.Boolean]) extends TypeAdapter[java.lang.Boolean] {

  override def read(reader: Reader): java.lang.Boolean =
    reader.peek match {
      case TokenType.False | TokenType.True =>
        java.lang.Boolean.valueOf(reader.readBoolean())

      case TokenType.Null =>
        reader.readNull()

      case actual =>
        reader.read()
        throw new IllegalStateException(s"Expected value token of type True or False, not $actual when reading Boolean value.  (Is your value wrapped in quotes?)\n" + reader.showError())
    }

  override def write(value: java.lang.Boolean, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeBoolean(value.booleanValue)
    }

}
