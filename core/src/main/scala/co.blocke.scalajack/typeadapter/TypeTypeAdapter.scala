package co.blocke.scalajack
package typeadapter

import scala.reflect.runtime.universe.{ Mirror, Type, TypeTag, typeOf }
import scala.language.existentials

object TypeTypeAdapter extends TypeAdapterFactory {

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (tt.tpe =:= typeOf[Type]) {
      TypeTypeAdapter(tt.mirror).asInstanceOf[TypeAdapter[T]]
    } else {
      next.typeAdapterOf[T]
    }

}

case class TypeTypeAdapter(mirror: Mirror, crashOnNotFound: Boolean = true) extends TypeAdapter[Type] {

  override def read(reader: Reader): Type =
    reader.peek match {
      case TokenType.String =>
        val fullName = reader.readString()
        try {
          mirror.staticClass(fullName).toType
        } catch {
          case e: ScalaReflectionException =>
            if (crashOnNotFound) // Normally... die if class not found
              throw new ClassNotFoundException(s"""Unable to find class named "$fullName"\n""" + reader.showError(), e)
            else // Except for "externalized types" (type members), where we may have ParseOrElse behavior later
              typeOf[UnknownType]
        }

      case TokenType.Null =>
        reader.readNull()
    }

  override def write(value: Type, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      val fullName = value.typeSymbol.fullName
      writer.writeString(fullName)
    }

}
