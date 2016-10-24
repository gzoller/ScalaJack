package co.blocke.scalajack
package typeadapter

import scala.reflect.runtime.universe.{ NoType, TypeTag, typeOf }

object OptionTypeAdapter extends TypeAdapterFactory {

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    tt.tpe.baseType(typeOf[Option[_]].typeSymbol) match {
      case NoType =>
        next.typeAdapterOf[T]

      case asOption =>
        val valueType :: Nil = asOption.typeArgs
        val valueTypeAdapter = context.typeAdapter(valueType)
        OptionTypeAdapter(valueTypeAdapter).asInstanceOf[TypeAdapter[T]]
    }

}

// We need 3 types of Option adapters here:
//   1: The "normal" one writes nothing for None.  This is used most places: Class members, list items, Map values
//   2: "Empty" one writes "" for None.  This is used for Map keys that are None
//   3: "Null" one converts None into null.  This is used mainly for Tuple members
//

case class OptionTypeAdapter[T](valueTypeAdapter: TypeAdapter[T]) extends TypeAdapter[Option[T]] {

  override def defaultValue: Option[Option[T]] = Some(None)

  override def read(reader: Reader): Option[T] =
    reader.peek match {
      case TokenType.Nothing | TokenType.Null =>
        reader.read()
        None
      case v => Some(valueTypeAdapter.read(reader))
    }

  override def write(optionalValue: Option[T], writer: Writer): Unit =
    optionalValue match {
      case null =>
        writer.writeNull()

      case Some(value) =>
        valueTypeAdapter.write(value, writer)

      case None =>
        writer.writeNothing()
    }

  // Must be called by parent of the Option when appropriate to get the null-writing version.
  def nullVersion(): TypeAdapter[Option[T]] = OptionTypeAdapterNull(valueTypeAdapter)
  def emptyVersion(): TypeAdapter[Option[T]] = OptionTypeAdapterEmpty(valueTypeAdapter)

}

case class OptionTypeAdapterNull[T](valueTypeAdapter: TypeAdapter[T]) extends TypeAdapter[Option[T]] {

  override def read(reader: Reader): Option[T] =
    reader.peek match {
      case TokenType.Nothing | TokenType.Null =>
        reader.read()
        None
      case v => Some(valueTypeAdapter.read(reader))
    }

  override def write(optionalValue: Option[T], writer: Writer): Unit =
    optionalValue match {
      case null =>
        writer.writeNull()

      case Some(value) =>
        valueTypeAdapter.write(value, writer)

      case None =>
        writer.writeNull()
    }

}

// This is for noncanonical map keys, which can be None --> rendered as ""
// Reads have to reverse-engineer the "" into a None
case class OptionTypeAdapterEmpty[T](valueTypeAdapter: TypeAdapter[T]) extends TypeAdapter[Option[T]] {

  // Special logic for TokenType.String.  A map key of value None for any value type (Int, ...) will be "".
  // So we must test for String and infer None if value is "".  Note that means there's no real way to 
  // infer Some("") as this will always devolve into None.  Have to make a compromose somewhere.
  private def read(reader: Reader, testStringForNull: Boolean): Option[T] = {
    reader.peek match {
      case TokenType.Nothing | TokenType.Null | TokenType.End =>
        reader.read()
        None
      case TokenType.String if (testStringForNull) =>
        val savedPos = reader.position
        if (reader.readString() == "")
          None
        else {
          reader.position = savedPos
          read(reader, false)
        }
      case v =>
        valueTypeAdapter.read(reader) match {
          case res => Some(res)
        }
    }
  }

  override def read(reader: Reader): Option[T] =
    read(reader, true)

  override def write(optionalValue: Option[T], writer: Writer): Unit =
    optionalValue match {
      case null =>
        writer.writeNull()

      case Some(value) =>
        valueTypeAdapter.write(value, writer)

      case None =>
        writer.writeString("")
    }

}
