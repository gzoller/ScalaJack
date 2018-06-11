package co.blocke.scalajack
package typeadapter

import scala.language.existentials
import scala.reflect.runtime.universe.{ Mirror, Type, TypeTag, typeOf }

object TypeTypeAdapter extends TypeAdapterFactory {

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] = {
    if (tt.tpe =:= typeOf[Type]) {
      TypeTypeAdapter(tt.mirror).asInstanceOf[TypeAdapter[T]]
    } else {
      next.typeAdapterOf[T]
    }
  }

}

case class TypeTypeAdapter(mirror: Mirror, typeModifier: Option[HintModifier] = None) extends TypeAdapter[Type] {

  override def read(reader: Reader): Type =
    reader.peek match {
      case TokenType.String =>
        typeModifier.map(mod => mod.apply(reader.readString())).getOrElse {
          val fullName = reader.readString()
          try {
            mirror.staticClass(fullName).toType
          } catch {
            case e: ScalaReflectionException =>
              throw new ClassNotFoundException(s"""Unable to find class named "$fullName"\n""" + reader.showError(), e)
          }
        }

      case TokenType.Null =>
        // $COVERAGE-OFF$Safety check--not used
        reader.readNull()
      // $COVERAGE-ON$
    }

  override def write(value: Type, writer: Writer): Unit =
    if (value == null) {
      // $COVERAGE-OFF$Safety check--not used
      writer.writeNull()
      // $COVERAGE-ON$
    } else {
      writer.writeString(typeModifier.map(mod => mod.unapply(value)).getOrElse(value.typeSymbol.fullName))
    }

}
