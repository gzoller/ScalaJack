package co.blocke.scalajack
package typeadapter
package javaprimitives

object JavaDoubleTypeAdapter extends TypeAdapterFactory.=:=[java.lang.Double] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[java.lang.Double]): TypeAdapter[java.lang.Double] = {
    val doubleTypeAdapter = context.typeAdapterOf[Double]
    new JavaDoubleTypeAdapter(
      deserializer = new BoxedDoubleDeserializer(doubleTypeAdapter.deserializer),
      serializer   = new BoxedDoubleSerializer(doubleTypeAdapter.serializer))
  }

}

class JavaDoubleTypeAdapter(override val deserializer: Deserializer[java.lang.Double], override val serializer: Serializer[java.lang.Double]) extends TypeAdapter.=:=[java.lang.Double]
