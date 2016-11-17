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
    reader.skipValue
    // Figure out base type of the Unknown Type.
    // Look up that base type in a parseOrElse cache to see if a substitute object is specified.
    // Provide substitute object or exception if not found.
    null.asInstanceOf[UnknownType]
  }

  override def write(value: Any, writer: Writer): Unit = {
    // No implementation...  Logically never present on write.
  }

}
