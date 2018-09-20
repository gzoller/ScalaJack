package co.blocke.scalajack
package typeadapter
package javaprimitives

object JavaBooleanTypeAdapter extends TypeAdapterFactory.=:=[java.lang.Boolean] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[java.lang.Boolean]): TypeAdapter[java.lang.Boolean] = {
    val booleanTypeAdapter = context.typeAdapterOf[Boolean]
    new JavaBooleanTypeAdapter(
      deserializer = new BoxedBooleanDeserializer(booleanTypeAdapter.deserializer),
      serializer   = new BoxedBooleanSerializer(booleanTypeAdapter.serializer))
  }

}

class JavaBooleanTypeAdapter(override val deserializer: Deserializer[java.lang.Boolean], override val serializer: Serializer[java.lang.Boolean]) extends TypeAdapter[java.lang.Boolean]