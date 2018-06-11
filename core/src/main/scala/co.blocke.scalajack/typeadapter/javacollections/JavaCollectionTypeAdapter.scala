package co.blocke.scalajack.typeadapter
package javacollections

import co.blocke.scalajack.{ Context, DeserializationError, DeserializationFailure, DeserializationResult, DeserializationSuccess, Deserializer, JsonArray, JsonNull, JsonOps, Path, Reader, SerializationResult, Serializer, TypeAdapter, TypeAdapterFactory, TypeTagged, Writer }

import scala.reflect.runtime.universe.{ Type, TypeTag }

object JavaCollectionTypeAdapter extends TypeAdapterFactory.<:<.withOneTypeParam[java.util.Collection] {

  override def create[E, C <: java.util.Collection[E]](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[C], ttCollection: TypeTag[java.util.Collection[E]], ttElement: TypeTag[E]): TypeAdapter[C] = {
    val elementTypeAdapter = context.typeAdapterOf[E]

    new JavaCollectionTypeAdapter[E, C](
      deserializer = new JavaCollectionDeserializer[E, C](elementTypeAdapter.deserializer, () => ???),
      serializer   = new JavaCollectionSerializer[E, C](elementTypeAdapter.serializer))
  }

}

class JavaCollectionDeserializer[E, C <: java.util.Collection[E]](elementDeserializer: Deserializer[E], newEmptyCollection: () => C)(implicit tt: TypeTag[C]) extends Deserializer[C] {

  private val collectionType: Type = tt.tpe
  private val nullTypeTagged: TypeTagged[C] = TypeTagged(null.asInstanceOf[C], collectionType)

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[C] =
    json match {
      case JsonNull() =>
        DeserializationSuccess(nullTypeTagged)

      case JsonArray(x) =>
        val elementsJson = x.asInstanceOf[ops.ArrayElements]

        DeserializationResult(path)({
          val collection: C = newEmptyCollection()

          ops.foreachArrayElement(elementsJson, { (index, elementJson) =>
            val DeserializationSuccess(TypeTagged(element)) = elementDeserializer.deserialize(path \ index, elementJson)
            collection.add(element)
          })

          TypeTagged(collection, collectionType)
        })

      case _ =>
        DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON array"))
    }

}

class JavaCollectionSerializer[E, C <: java.util.Collection[E]](elementSerializer: Serializer[E]) extends Serializer[C] {

  override def serialize[J](tagged: TypeTagged[C])(implicit ops: JsonOps[J]): SerializationResult[J] = ???

}

class JavaCollectionTypeAdapter[E, C <: java.util.Collection[E]](override val deserializer: Deserializer[C], override val serializer: Serializer[C]) extends TypeAdapter[C] {

  override def read(reader: Reader): C = ???

  override def write(value: C, writer: Writer): Unit = ???

}
