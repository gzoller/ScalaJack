package co.blocke.scalajack

import scala.reflect.ClassTag

object TypeAdapter {

  abstract class ===[X](implicit ttFactory: TypeTag[X]) extends TypeAdapterFactory.===[X] with TypeAdapter[X] {

    override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[X]): TypeAdapter[X] = this

  }

  abstract class =:=[X](implicit ttFactory: TypeTag[X]) extends TypeAdapterFactory.=:=[X] with TypeAdapter[X] {

    override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[X]): TypeAdapter[X] = this

  }

  object =:= {

    class constant[X](tagged: TypeTagged[X])(implicit ttFactory: TypeTag[X]) extends TypeAdapter.=:=[X] {
      override val deserializer: Deserializer[X] = Deserializer.constant(tagged)
      override val serializer: Serializer[X] = new Serializer[X] {
        override def serialize[J](tagged: TypeTagged[X])(implicit ops: JsonOps[J]): SerializationResult[J] =
          throw new UnsupportedOperationException(s"TypeAdapter.=:=.constant[${ttFactory.tpe}](...).serializer.serialize")
      }
    }

  }

  def apply[T](deserializer: Deserializer[T], serializer: Serializer[T]): TypeAdapter[T] = Fixed(deserializer, serializer)

  private case class Fixed[T](override val deserializer: Deserializer[T], override val serializer: Serializer[T]) extends TypeAdapter[T]

}

trait TypeAdapter[T] {

  self =>

  def is[U <: TypeAdapter[_]: ClassTag]: Boolean =
    maybeAs[U].isDefined

  def as[U <: TypeAdapter[_]: ClassTag]: U =
    maybeAs[U].getOrElse(throw new RuntimeException(s"$self is not an instance of ${implicitly[ClassTag[U]].runtimeClass}"))

  def maybeAs[U <: TypeAdapter[_]: ClassTag]: Option[U] = {
    val runtimeClass = implicitly[ClassTag[U]].runtimeClass
    try {
      Some(runtimeClass.cast(self).asInstanceOf[U])
    } catch {
      case e: ClassCastException =>
        None
    }
  }

  val deserializer: Deserializer[T] = new Deserializer[T] {
    override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J], guidance: DeserializationGuidance): DeserializationResult[T] =
      throw new NotImplementedError(s"$self.deserializer.deserialize")
  }

  val serializer: Serializer[T] = new Serializer[T] {
    override def serialize[J](tagged: TypeTagged[T])(implicit ops: JsonOps[J]): SerializationResult[J] =
      throw new NotImplementedError(s"$self.serializer.serialize")
  }

  def andThen[U](f: BijectiveFunction[T, U])(implicit context: Context, ttB: TypeTag[U]): TransformedTypeAdapter[T, U] =
    TransformedTypeAdapter(this, f)

  // $COVERAGE-OFF$Tested in concrete classes, not here
  def defaultValue: Option[T] = None
  // $COVERAGE-ON$

  def resolved: TypeAdapter[T] = this
}

// Marker trait for those TypeAdapters which render as String
// (Determines if a value will be wrapped in quotes or not for noncanonical
// processing in NoncanonicalMapKeyParsingTypeAdapter)
trait StringKind
