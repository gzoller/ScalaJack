package co.blocke.scalajack
package typeadapter
package javaprimitives

object JavaBigDecimalTypeAdapter extends TypeAdapterFactory.=:=[java.math.BigDecimal] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[java.math.BigDecimal]): TypeAdapter[java.math.BigDecimal] = {
    val scalaBigDecimalTypeAdapter = context.typeAdapterOf[scala.math.BigDecimal]
    new JavaBigDecimalTypeAdapter(
      deserializer = new JavaBigDecimalDeserializer(scalaBigDecimalTypeAdapter.deserializer),
      serializer   = new JavaBigDecimalSerializer(scalaBigDecimalTypeAdapter.serializer))
  }

}

class JavaBigDecimalTypeAdapter(override val deserializer: Deserializer[java.math.BigDecimal], override val serializer: Serializer[java.math.BigDecimal]) extends TypeAdapter.=:=[java.math.BigDecimal]