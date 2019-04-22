package co.blocke.scalajack
package delimited

import model._
import util.Path

import scala.collection.mutable.Builder
import scala.reflect.runtime.universe.{ ClassSymbol, TypeTag }
import scala.util.{ Failure, Success, Try }

// $COVERAGE-OFF$Exactly the same as EnumerationTypeAdapterFactory--don't need to test twice!
object DelimitedEnumerationTypeAdapterFactory extends TypeAdapterFactory.FromClassSymbol {

  override def typeAdapterOf[T](classSymbol: ClassSymbol, next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (tt.tpe.typeSymbol.fullName == "scala.Enumeration.Value") {
      // Can't use tpe <:< because Enumeration has no companion object
      val erasedEnumClassName = tt.tpe.toString match {
        case raw if (raw.endsWith(".Value")) => raw.replace(".Value", "$")
        case raw                             => raw.dropRight(raw.length - raw.lastIndexOf('.')) + "$"
      }
      val enum = Class.forName(erasedEnumClassName).getField(scala.reflect.NameTransformer.MODULE_INSTANCE_NAME).get(null).asInstanceOf[Enumeration]
      DelimitedEnumerationTypeAdapter(enum).asInstanceOf[TypeAdapter[T]]
    } else {
      next.typeAdapterOf[T]
    }
}
// $COVERAGE-ON$

case class DelimitedEnumerationTypeAdapter[E <: Enumeration](enum: E) extends TypeAdapter[E#Value] {

  def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): E#Value =
    reader.head.tokenType match {
      case TokenType.String | TokenType.QuotedString =>
        val strVal = reader.readString(path)
        val tryVal = if (strVal forall Character.isDigit)
          Try(enum(strVal.toInt))
        else
          Try(enum.withName(strVal))
        tryVal match {
          case Success(u) => u
          case Failure(u) =>
            reader.back
            throw new ReadInvalidError(reader.showError(path, s"No value found in enumeration ${enum.getClass.getName} for ${reader.head.textValue}"))
        }
    }

  // $COVERAGE-OFF$Exactly the same as EnumerationTypeAdapterFactory--don't need to test twice!
  def write[WIRE](t: E#Value, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit =
    t match {
      case null                                => writer.writeNull(out)
      case v if (writer.jackFlavor.enumsAsInt) => writer.writeInt(v.id, out)
      case v                                   => writer.writeString(v.toString, out)
    }
  // $COVERAGE-ON$
}

