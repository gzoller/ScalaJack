package co.blocke.scalajack

import scala.reflect.ClassTag

object TermTypeAdapterFactory extends TypeAdapterFactory {

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] = {
    val ta = next.typeAdapterOf[T]
    val deserializer = new TermDeserializer[T](ta.deserializer)
    val serializer = new TermSerializer(ta.serializer)
    new TermTypeAdapter[T](deserializer, serializer, ta)
  }

}

class TermTypeAdapter[T](override val deserializer: Deserializer[T], override val serializer: Serializer[T], next: TypeAdapter[T]) extends TypeAdapter[T] {

  self =>

  override def as[U <: TypeAdapter[_]: ClassTag]: U =
    maybeAs[U].getOrElse(throw new RuntimeException(s"Neither $self nor $next is an instance of ${implicitly[ClassTag[U]].runtimeClass}"))

  override def maybeAs[U <: TypeAdapter[_]: ClassTag]: Option[U] =
    super.maybeAs orElse next.maybeAs[U]

}
