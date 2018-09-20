package co.blocke.scalajack
package typeadapter
package javaprimitives

object JavaBigIntegerTypeAdapter extends TypeAdapterFactory.=:=[java.math.BigInteger] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[java.math.BigInteger]): TypeAdapter[java.math.BigInteger] = {
    val scalaBigIntTypeAdapter = context.typeAdapterOf[scala.math.BigInt]
    new JavaBigIntegerTypeAdapter(
      deserializer = new JavaBigIntegerDeserializer(scalaBigIntTypeAdapter.deserializer),
      serializer   = new JavaBigIntegerSerializer(scalaBigIntTypeAdapter.serializer))
  }

}

class JavaBigIntegerTypeAdapter(override val deserializer: Deserializer[java.math.BigInteger], override val serializer: Serializer[java.math.BigInteger]) extends TypeAdapter[java.math.BigInteger]