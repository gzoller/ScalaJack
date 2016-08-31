package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{ Context, Reader, TypeAdapter, TypeAdapterFactory, UnreadableJsonException, Writer }

import scala.util.{ Failure, Success, Try }
import scala.reflect.runtime.universe.{ Type, typeOf }

object TryTypeAdapter extends TypeAdapterFactory {

  override def typeAdapter(tpe: Type, context: Context): Option[TypeAdapter[_]] =
    if (tpe <:< typeOf[Try[_]]) {
      val valueType = tpe.typeArgs.head
      val valueTypeAdapter = context.typeAdapter(valueType)

      Some(TryTypeAdapter(valueTypeAdapter))
    } else {
      None
    }

}

case class TryTypeAdapter[T](valueTypeAdapter: TypeAdapter[T]) extends TypeAdapter[Try[T]] {

  override def read(reader: Reader): Try[T] = {
    val originalPosition = reader.position

    val attempt = Try { valueTypeAdapter.read(reader) }

    attempt match {
      case self @ Success(_) ⇒
        self

      case Failure(cause) ⇒
        reader.position = originalPosition
        reader.skipValue()

        val unreadableJsonOffset = reader.tokenOffsetAt(originalPosition)
        val unreadableJsonLength = reader.tokenOffsetAt(reader.position) - unreadableJsonOffset

        val exception = new UnreadableJsonException(cause) {
          override def write(writer: Writer): Unit = {
            writer.writeRawValue(reader.source, unreadableJsonOffset, unreadableJsonLength)
          }
        }

        Failure(exception)
    }
  }

  override def write(value: Try[T], writer: Writer): Unit =
    value match {
      case Success(v) ⇒
        valueTypeAdapter.write(v, writer)

      case Failure(e: UnreadableJsonException) ⇒
        e.write(writer)

      case Failure(e) ⇒
        throw e
    }

}
