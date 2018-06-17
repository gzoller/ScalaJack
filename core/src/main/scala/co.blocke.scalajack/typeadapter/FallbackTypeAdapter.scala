package co.blocke.scalajack
package typeadapter

import scala.util.{ Failure, Success, Try }

case class FallbackTypeAdapter[T](
    override val deserializer: Deserializer[T],
    override val serializer:   Serializer[T],
    primaryTypeAdapter:        TypeAdapter[T],
    secondaryTypeAdapter:      TypeAdapter[T]) extends TypeAdapter[T] {

  override def read(reader: Reader): T = {
    val originalPosition = reader.position

    val attempt = Try { primaryTypeAdapter.read(reader) }

    attempt match {
      case Success(value) =>
        value

      case Failure(_) =>
        // Let's try that again...
        reader.position = originalPosition
        secondaryTypeAdapter.read(reader)
    }
  }

  // $COVERAGE-OFF$Should never be used semantically
  // Should never be called... fallback object "becomes" the asked-for object
  override def write(value: T, writer: Writer): Unit =
    primaryTypeAdapter.write(value, writer)
  // $COVERAGE-ON$

}
