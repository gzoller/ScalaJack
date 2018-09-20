package co.blocke.scalajack
package typeadapter
package javaprimitives

object JavaCharacterTypeAdapter extends TypeAdapterFactory.=:=[java.lang.Character] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[java.lang.Character]): TypeAdapter[java.lang.Character] = {
    val charTypeAdapter = context.typeAdapterOf[Char]
    new JavaCharacterTypeAdapter(
      deserializer = new BoxedCharDeserializer(charTypeAdapter.deserializer),
      serializer   = new BoxedCharSerializer(charTypeAdapter.serializer))
  }

}

class JavaCharacterTypeAdapter(override val deserializer: Deserializer[java.lang.Character], override val serializer: Serializer[java.lang.Character]) extends TypeAdapter.=:=[java.lang.Character] with StringKind
