package co.blocke.scalajack
package typeadapter

import co.blocke.scalajack.BijectiveFunctions.fullNameToType

import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{ Type, TypeTag, typeOf }

trait UnknownType // marker trait

object UnknownTypeTypeAdapter extends TypeAdapterFactory {

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (tt.tpe =:= typeOf[UnknownType]) {
      UnknownTypeTypeAdapter().asInstanceOf[TypeAdapter[T]]
    } else {
      next.typeAdapterOf[T]
    }

}

case class UnknownTypeTypeAdapter( // typeTypeAdapter:       TypeAdapter[Type],
// memberNameTypeAdapter: TypeAdapter[MemberName],
// mapTypeAdapter:        TypeAdapter[Map[Any, Any]],
// listTypeAdapter:       TypeAdapter[List[Any]],
// stringTypeAdapter:     TypeAdapter[String],
// booleanTypeAdapter:    TypeAdapter[Boolean],
// context:               Context
) extends SimpleTypeAdapter[Any] {

  override def read(reader: Reader): UnknownType = {
    /*
    reader.peek match {
      case TokenType.BeginObject =>
        val savedPos = reader.position
        val mapRead = mapTypeAdapter.read(reader)
        // See if it's a serialized class (with default type hint).  Create class if so.
        val optionalClassType = mapRead.get(context.defaultHint).map(hint => fullNameToType.apply(hint.asInstanceOf[String]))
        optionalClassType match {
          case Some(t) =>
            reader.position = savedPos
            val classConstructed = context.typeAdapter(t).read(reader)
            classConstructed
          case None => mapRead
        }

      case TokenType.BeginArray =>
        listTypeAdapter.read(reader)

      case TokenType.String =>
        stringTypeAdapter.read(reader)

      case TokenType.True | TokenType.False =>
        booleanTypeAdapter.read(reader)

      case TokenType.Number =>
        reader.readNumber() // Use Scala numerical inference (see Reader.readNumber())

      case TokenType.Null =>
        reader.readNull()

    }
    */
    reader.skipValue
    null.asInstanceOf[UnknownType]
  }

  override def write(value: Any, writer: Writer): Unit = {
    // No implementation...  Logically never present on write.
  }

}
