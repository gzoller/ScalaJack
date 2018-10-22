package co.blocke.scalajack
package typeadapter
package javacollections

object JavaMapTypeAdapter extends TypeAdapterFactory.<:<.withTwoTypeParams[java.util.Map] {

  override def create[K, V, M <: java.util.Map[K, V]](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[M], ttMap: TypeTag[java.util.Map[K, V]], ttKey: TypeTag[K], ttValue: TypeTag[V]): TypeAdapter[M] = {

    val mapConstructor: java.lang.reflect.Constructor[M] = runtimeClassOf[M].getConstructor()

    def newEmptyMap(): M = mapConstructor.newInstance()

    val keyTypeAdapter = context.typeAdapterOf[K]
    val valueTypeAdapter = context.typeAdapterOf[V]

    new JavaMapTypeAdapter[K, V, M](
      deserializer = new JavaMapDeserializer[K, V, M](keyTypeAdapter.deserializer, valueTypeAdapter.deserializer, (() => newEmptyMap())),
      serializer   = new JavaMapSerializer[K, V, M](keyTypeAdapter.serializer, valueTypeAdapter.serializer))
  }

}

class JavaMapTypeAdapter[K, V, M <: java.util.Map[K, V]](override val deserializer: Deserializer[M], override val serializer: Serializer[M]) extends TypeAdapter[M]
