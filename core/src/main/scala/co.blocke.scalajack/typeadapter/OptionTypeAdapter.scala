package co.blocke.scalajack
package typeadapter

import scala.reflect.runtime.universe.{ Type, typeOf }

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

  override def defaultValue: Option[Option[T]] = Some(None)

  override def read(reader: Reader): Option[T] =
    if (reader.peek == TokenType.Nothing) {
      None
    } else {
      valueTypeAdapter.read(reader) match {
        case null ⇒ null
        case v    ⇒ Some(v)
      }
    }

  override def write(optionalValue: Option[T], writer: Writer): Unit =
    optionalValue match {
      case null ⇒
        writer.writeNull()

      case Some(value) ⇒
        valueTypeAdapter.write(value, writer)

      case None ⇒
        writer.writeNothing()
    }

}
