package co.blocke.scalajack

trait TypeAdapter[T] {

  def read(reader: Reader): T

  def write(value: T, writer: Writer): Unit

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
    f:           BijectiveFunction[A, B]
) extends TypeAdapter[B] {

  override def read(reader: Reader): B =
    f.apply(typeAdapter.read(reader))

  override def write(value: B, writer: Writer): Unit =
    typeAdapter.write(f.unapply(value), writer)

}
