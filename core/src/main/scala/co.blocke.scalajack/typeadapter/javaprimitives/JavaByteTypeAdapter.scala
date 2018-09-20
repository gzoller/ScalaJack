package co.blocke.scalajack
package typeadapter
package javaprimitives

object JavaByteTypeAdapter extends TypeAdapterFactory.=:=[java.lang.Byte] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[java.lang.Byte]): TypeAdapter[java.lang.Byte] = {
    val byteTypeAdapter = context.typeAdapterOf[Byte]
    new JavaByteTypeAdapter(
      deserializer = new BoxedByteDeserializer(byteTypeAdapter.deserializer),
      serializer   = new BoxedByteSerializer(byteTypeAdapter.serializer))
  }

}

class JavaByteTypeAdapter(override val deserializer: Deserializer[java.lang.Byte], override val serializer: Serializer[java.lang.Byte]) extends TypeAdapter.=:=[java.lang.Byte]
