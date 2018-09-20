package co.blocke.scalajack
package typeadapter

import scala.language.existentials

object TypeTypeAdapter extends TypeAdapterFactory {

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] = {
    if (tt.tpe =:= typeOf[Type]) {
      TypeTypeAdapter(
        new TypeDeserializer,
        new TypeSerializer,
        tt.mirror).asInstanceOf[TypeAdapter[T]]
    } else {
      next.typeAdapterOf[T]
    }
  }

}

case class TypeTypeAdapter(override val deserializer: Deserializer[Type], override val serializer: Serializer[Type], mirror: Mirror, typeModifier: Option[HintModifier] = None) extends TypeAdapter[Type]
