package co.blocke.scalajack
package typeadapter

import scala.util.{ Failure, Success, Try }

object EnumerationTypeAdapter extends TypeAdapterFactory.FromClassSymbol {

  override def typeAdapterOf[T](classSymbol: ClassSymbol, next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (tt.tpe.typeSymbol.fullName == "scala.Enumeration.Value") {
      // Can't use tpe <:< because Enumeration has no companion object
      val erasedEnumClassName = tt.tpe.toString match {
        case raw if (raw.endsWith(".Value")) => raw.replace(".Value", "$")
        case raw                             => raw.dropRight(raw.length - raw.lastIndexOf('.')) + "$"
      }
      val enum = Class.forName(erasedEnumClassName).getField(scala.reflect.NameTransformer.MODULE_INSTANCE_NAME).get(null).asInstanceOf[Enumeration]
      EnumerationTypeAdapter(
        new EnumerationValueDeserializer(enum),
        new EnumerationValueSerializer,
        enum).asInstanceOf[TypeAdapter[T]]
    } else {
      next.typeAdapterOf[T]
    }

}

case class EnumerationTypeAdapter[E <: Enumeration](override val deserializer: Deserializer[E#Value], override val serializer: Serializer[E#Value], enum: E) extends TypeAdapter[E#Value] with StringKind
