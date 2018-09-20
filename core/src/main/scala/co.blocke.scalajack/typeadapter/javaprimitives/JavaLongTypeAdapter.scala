package co.blocke.scalajack
package typeadapter
package javaprimitives

object JavaLongTypeAdapter extends TypeAdapterFactory.=:=[java.lang.Long] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[java.lang.Long]): TypeAdapter[java.lang.Long] = {
    val longTypeAdapter = context.typeAdapterOf[Long]
    new JavaLongTypeAdapter(
      deserializer = new BoxedLongDeserializer(longTypeAdapter.deserializer),
      serializer   = new BoxedLongSerializer(longTypeAdapter.serializer))
  }

}

class JavaLongTypeAdapter(override val deserializer: Deserializer[java.lang.Long], override val serializer: Serializer[java.lang.Long]) extends TypeAdapter[java.lang.Long]