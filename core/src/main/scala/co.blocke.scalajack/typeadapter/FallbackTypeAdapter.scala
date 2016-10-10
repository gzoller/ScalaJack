package co.blocke.scalajack
package typeadapter

import scala.util.{ Failure, Success, Try }

case class FallbackTypeAdapter[T](
    primaryTypeAdapter:   TypeAdapter[T],
    secondaryTypeAdapter: TypeAdapter[T]
) extends TypeAdapter[T] {

  override def read(reader: Reader): T = {
    val originalPosition = reader.position

    val attempt = Try { primaryTypeAdapter.read(reader) }

    attempt match {
      case Success(value) ⇒
        value

      case Failure(_) ⇒
        // Let's try that again...
        reader.position = originalPosition
        secondaryTypeAdapter.read(reader)
    }
  }

  // $COVERAGE-OFF$Disabling highlighting by default until a workaround for https://issues.scala-lang.org/browse/SI-8596 is found
  // Should never be called... fallback object "becomes" the asked-for object
  override def write(value: T, writer: Writer): Unit =
    primaryTypeAdapter.write(value, writer)
  // $COVERAGE-ON$

}
