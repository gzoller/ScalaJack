package co.blocke.scalajack
package typeadapter
package javaprimitives

object JavaNumberTypeAdapter extends TypeAdapterFactory.=:=[java.lang.Number] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[java.lang.Number]): TypeAdapter[Number] =
    new JavaNumberTypeAdapter(deserializer = new BoxedNumberDeserializer, serializer = new BoxedNumberSerializer)

}

class JavaNumberTypeAdapter(override val deserializer: Deserializer[java.lang.Number], override val serializer: Serializer[java.lang.Number]) extends SimpleTypeAdapter.ForTypeSymbolOf[java.lang.Number]
