package co.blocke.scalajack
package typeadapter

import model._
import util.Path

import scala.collection.mutable.Builder
import scala.reflect.runtime.universe.{ ClassSymbol, TypeTag }
import scala.util.{ Failure, Success, Try }

object EnumerationTypeAdapterFactory extends TypeAdapterFactory.FromClassSymbol {

  override def typeAdapterOf[T](classSymbol: ClassSymbol, next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (tt.tpe.typeSymbol.fullName == "scala.Enumeration.Value") {
      // Can't use tpe <:< because Enumeration has no companion object
      val erasedEnumClassName = tt.tpe.toString match {
        case raw if (raw.endsWith(".Value")) => raw.replace(".Value", "$")
        case raw                             => raw.dropRight(raw.length - raw.lastIndexOf('.')) + "$"
      }
      val enum = Class.forName(erasedEnumClassName).getField(scala.reflect.NameTransformer.MODULE_INSTANCE_NAME).get(null).asInstanceOf[Enumeration]
      EnumerationTypeAdapter(enum).asInstanceOf[TypeAdapter[T]]
    } else {
      next.typeAdapterOf[T]
    }

}

case class EnumerationTypeAdapter[E <: Enumeration](enum: E) extends TypeAdapter[E#Value] with Stringish {

  def read[WIRE](path: Path, reader: Transceiver[WIRE]): E#Value =
    reader.peek match {
      case TokenType.String =>
        Try(enum.withName(reader.readString(path))) match {
          case Success(u) => u
          case Failure(u) =>
            throw new ReadInvalidError(path, s"No value found in enumeration ${enum.getClass.getName} for ${reader.lastTokenText}\n" + reader.showError(), List(enum.getClass.getName, reader.lastTokenText))
        }
      case TokenType.Number =>
        Try(enum(reader.readInt(path))) match {
          case Success(u) => u
          case Failure(u) =>
            throw new ReadInvalidError(path, s"No value found in enumeration ${enum.getClass.getName} for ${reader.lastTokenText}\n" + reader.showError(), List(enum.getClass.getName, reader.lastTokenText))
        }
      case TokenType.Null =>
        reader.skip()
        null
      case actual =>
        reader.skip()
        throw new ReadUnexpectedError(path, s"Expected value token of type String or Int, not $actual when reading Enumeration value.\n" + reader.showError, List(actual.toString))
    }

  def write[WIRE](t: E#Value, writer: Transceiver[WIRE], out: Builder[Any, WIRE], isMapKey: Boolean): Unit =
    t match {
      case null => writer.writeNull(out)
      case v    => writer.writeString(v.toString, out)
    }
}
