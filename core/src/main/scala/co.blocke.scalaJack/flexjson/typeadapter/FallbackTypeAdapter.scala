package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{Reader, TypeAdapter, Writer}

import scala.util.{Failure, Success, Try}

case class FallbackTypeAdapter[T](primaryTypeAdapter: TypeAdapter[T],
                                  secondaryTypeAdapter: TypeAdapter[T]) extends TypeAdapter[T] {

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

  override def write(value: T, writer: Writer): Unit =
    primaryTypeAdapter.write(value, writer)

}
