package co.blocke.scalajack
package typeadapter
package javaprimitives

object JavaIntegerTypeAdapter extends TypeAdapterFactory.=:=[java.lang.Integer] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[java.lang.Integer]): TypeAdapter[java.lang.Integer] = {
    val intTypeAdapter = context.typeAdapterOf[Int]
    new JavaIntegerTypeAdapter(
      deserializer = new BoxedIntDeserializer(intTypeAdapter.deserializer),
      serializer   = new BoxedIntSerializer(intTypeAdapter.serializer))
  }

}

class JavaIntegerTypeAdapter(override val deserializer: Deserializer[java.lang.Integer], override val serializer: Serializer[java.lang.Integer]) extends TypeAdapter[java.lang.Integer]
