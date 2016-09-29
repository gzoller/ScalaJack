package co.blocke.scalajack

trait TypeAdapter[T] {

  val isStringKind: Boolean = false

  def read(reader: Reader): T

  def write(value: T, writer: Writer): Unit

  def andThen[U](f: BijectiveFunction[T, U]): TransformedTypeAdapter[T, U] =
    TransformedTypeAdapter(this, f)

  def defaultValue: Option[T] = None

}

case class TransformedTypeAdapter[A, B](
    typeAdapter: TypeAdapter[A],
    f:           BijectiveFunction[A, B]
) extends TypeAdapter[B] {

  override def read(reader: Reader): B =
    f.apply(typeAdapter.read(reader))

  override def write(value: B, writer: Writer): Unit =
    typeAdapter.write(f.unapply(value), writer)

}
