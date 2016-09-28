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

// We need 2 types of Option adapters here.  The "normal" one writes nothing for None.
// This is used most places: Class members, list items, Map values.
//
// The second kind writes None as null.  This is used for naked Option and Tuples.
//
// Both flavors promote null -> None on read

case class OptionTypeAdapter[T](valueTypeAdapter: TypeAdapter[T]) extends TypeAdapter[Option[T]] {

  override def read(reader: Reader): Option[T] =
    reader.peek match {
      case TokenType.Nothing | TokenType.Null ⇒ None
      case v                                  ⇒ Some(valueTypeAdapter.read(reader))
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

  // Must be called by parent of the Option when appropriate to get the null-writing version.
  def nullVersion(): TypeAdapter[Option[T]] = OptionTypeAdapterNull(valueTypeAdapter)

}

case class OptionTypeAdapterNull[T](valueTypeAdapter: TypeAdapter[T]) extends TypeAdapter[Option[T]] {

  override def read(reader: Reader): Option[T] =
    reader.peek match {
      case TokenType.Nothing | TokenType.Null ⇒ None
      case v                                  ⇒ Some(valueTypeAdapter.read(reader))
    }

  override def write(optionalValue: Option[T], writer: Writer): Unit =
    optionalValue match {
      case null ⇒
        writer.writeNull()

      case Some(value) ⇒
        valueTypeAdapter.write(value, writer)

      case None ⇒
        writer.writeNull()
    }

}
