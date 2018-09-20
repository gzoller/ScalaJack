package co.blocke.scalajack
package typeadapter
package javaprimitives

object JavaFloatTypeAdapter extends TypeAdapterFactory.=:=[java.lang.Float] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[java.lang.Float]): TypeAdapter[java.lang.Float] = {
    val floatTypeAdapter = context.typeAdapterOf[Float]
    new JavaFloatTypeAdapter(
      deserializer = new BoxedFloatDeserializer(floatTypeAdapter.deserializer),
      serializer   = new BoxedFloatSerializer(floatTypeAdapter.serializer))
  }

}

class JavaFloatTypeAdapter(override val deserializer: Deserializer[java.lang.Float], override val serializer: Serializer[java.lang.Float]) extends TypeAdapter.=:=[java.lang.Float]
