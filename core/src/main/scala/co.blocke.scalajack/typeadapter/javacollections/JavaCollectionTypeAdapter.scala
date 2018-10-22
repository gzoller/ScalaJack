package co.blocke.scalajack
package typeadapter
package javacollections

object JavaCollectionTypeAdapter extends TypeAdapterFactory.<:<.withOneTypeParam[java.util.Collection] {

  override def create[E, G <: java.util.Collection[E]](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[G], ttCollection: TypeTag[java.util.Collection[E]], ttElement: TypeTag[E]): TypeAdapter[G] = {
    val collectionConstructor: java.lang.reflect.Constructor[G] = runtimeClassOf[G].getConstructor()

    def newEmptyCollection(): G = collectionConstructor.newInstance()

    val elementTypeAdapter = context.typeAdapterOf[E]

    new JavaCollectionTypeAdapter[E, G](
      deserializer = new JavaCollectionDeserializer[E, G](elementTypeAdapter.deserializer, (() => newEmptyCollection())),
      serializer   = new JavaCollectionSerializer[E, G](elementTypeAdapter.serializer))
  }

}

class JavaCollectionTypeAdapter[E, G <: java.util.Collection[E]](override val deserializer: Deserializer[G], override val serializer: Serializer[G]) extends TypeAdapter[G]