package co.blocke.scalajack
package typeadapter

import scala.util.{ Failure, Success, Try }

object TryTypeAdapter extends TypeAdapterFactory.=:=.withOneTypeParam[Try] {

  override def create[E](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[Try[E]], ttElement: TypeTag[E]): TypeAdapter[Try[E]] =
    TryTypeAdapter(context.typeAdapterOf[E])

}

case class TryTypeAdapter[T](valueTypeAdapter: TypeAdapter[T]) extends TypeAdapter[Try[T]] {

  override def read(reader: Reader): Try[T] = {
    val originalPosition = reader.position

    val attempt = Try { valueTypeAdapter.read(reader) }

    attempt match {
      case self @ Success(_) =>
        self

      case Failure(cause) =>
        reader.position = originalPosition
        Failure(new UnreadableException(reader.captureValue(), cause))
    }
  }

  override def write(value: Try[T], writer: Writer): Unit =
    value match {
      case Success(v) =>
        valueTypeAdapter.write(v, writer)

      case Failure(e: UnreadableException) =>
        e.write(writer)

      case Failure(e) =>
        // $COVERAGE-OFF$USafety catch--shouldn't be possible
        throw e
      // $COVERAGE-ON$
    }

}
