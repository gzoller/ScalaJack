package co.blocke.scalajack
package typeadapter

import util.Path
import model._

import scala.collection.mutable.Builder

object TypeTypeAdapterFactory extends TypeAdapterFactory {

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] = {
    if (tt.tpe =:= typeOf[Type]) {
      TypeTypeAdapter(tt.mirror).asInstanceOf[TypeAdapter[T]]
    } else {
      next.typeAdapterOf[T]
    }
  }

}

trait HintModifier

case class TypeTypeAdapter(mirror: Mirror, typeModifier: Option[HintModifier] = None) extends TypeAdapter[Type] {

  def typeNameToType[WIRE](path: Path, typeName: String, reader: Reader[WIRE]): Type =
    try {
      staticClass(typeName).toType
    } catch {
      case e: ScalaReflectionException =>
        throw new ReadMissingError(reader.showError(path, s"""Unable to find class named "$typeName""""))
    }

  def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): Type = reader.readString(path) match {
    case s: String =>
      typeNameToType(path, s, reader)
    // No others should be possible
  }

  def write[WIRE](t: Type, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit =
    writer.writeString(t.typeSymbol.fullName, out)
}