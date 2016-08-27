package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{Context, Reader, TokenType, TypeAdapter, TypeAdapterFactory, Writer}

import scala.reflect.runtime.universe.{Type, typeOf}

object OptionTypeAdapter extends TypeAdapterFactory {

  override def typeAdapter(tpe: Type, context: Context): Option[TypeAdapter[_]] =
    if (tpe <:< typeOf[Option[_]]) {
      val valueType = tpe.typeArgs.head
      val valueTypeAdapter = context.typeAdapter(valueType)

      Some(OptionTypeAdapter(valueTypeAdapter))
    } else {
      None
    }

}

case class OptionTypeAdapter[T](valueTypeAdapter: TypeAdapter[T]) extends TypeAdapter[Option[T]] {

  override def read(reader: Reader): Option[T] =
    if (reader.peek == TokenType.Nothing) {
      None
    } else {
      Some(valueTypeAdapter.read(reader))
    }

  override def write(value: Option[T], writer: Writer): Unit = ???

}
