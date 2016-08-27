package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{Context, Reader, TypeAdapter, TypeAdapterFactory, Writer}

import scala.util.Try

import scala.reflect.runtime.universe.{Type, typeOf}

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

    if (attempt.isFailure) {
      reader.position = originalPosition
      reader.skipValue()
    }

    attempt
  }

  override def write(value: Try[T], writer: Writer): Unit = ???

}
