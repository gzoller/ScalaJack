package co.blocke.scalajack
package typeadapter
package javaprimitives

object JavaShortTypeAdapter extends TypeAdapterFactory.=:=[java.lang.Short] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[java.lang.Short]): TypeAdapter[java.lang.Short] = {
    val shortTypeAdapter = context.typeAdapterOf[Short]
    new JavaShortTypeAdapter(
      deserializer = new BoxedShortDeserializer(shortTypeAdapter.deserializer),
      serializer   = new BoxedShortSerializer(shortTypeAdapter.serializer))
  }

}

class JavaShortTypeAdapter(override val deserializer: Deserializer[java.lang.Short], override val serializer: Serializer[java.lang.Short]) extends TypeAdapter[java.lang.Short]
