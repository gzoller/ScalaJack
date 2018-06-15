package co.blocke.scalajack

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

  private case class Fixed[T](override val deserializer: Deserializer[T], override val serializer: Serializer[T]) extends TypeAdapter[T] {
    override def read(reader: Reader): T = ???

    override def write(value: T, writer: Writer): Unit = ???
  }

}

trait TypeAdapter[T] {

  self =>

  def read(reader: Reader): T = {
    val json = reader.readJsonValue()(Json4sOps)
    val DeserializationSuccess(TypeTagged(value)) = deserializer.deserialize(Path.Unknown, json)(Json4sOps)
    value
  }

  def write(value: T, writer: Writer): Unit = {
    val SerializationSuccess(json) = serializer.serialize(TypeTagged.inferFromRuntimeClass[T](value))(Json4sOps)
    writer.writeJsonValue(json)(Json4sOps)
  }

  val deserializer: Deserializer[T] = new Deserializer[T] {
    override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[T] =
      throw new NotImplementedError(s"$self.deserializer.deserialize")
  }

  val serializer: Serializer[T] = new Serializer[T] {
    override def serialize[J](tagged: TypeTagged[T])(implicit ops: JsonOps[J]): SerializationResult[J] =
      throw new NotImplementedError(s"$self.serializer.serialize")
  }

  def andThen[U](f: BijectiveFunction[T, U]): TransformedTypeAdapter[T, U] =
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

case class TransformedTypeAdapter[A, B](
    typeAdapter: TypeAdapter[A],
    f:           BijectiveFunction[A, B]) extends TypeAdapter[B] {

  override def read(reader: Reader): B =
    f.apply(typeAdapter.read(reader))

  override def write(value: B, writer: Writer): Unit =
    typeAdapter.write(f.unapply(value), writer)

}
