package co.blocke.scalajack
package typeadapter

import model._

import scala.collection.mutable
import scala.reflect.runtime.universe.{ ClassSymbol, TypeTag }
import scala.util.{ Failure, Success, Try }

object EnumerationTypeAdapterFactory
  extends TypeAdapterFactory.FromClassSymbol {

  override def typeAdapterOf[T](
      classSymbol: ClassSymbol,
      next:        TypeAdapterFactory
  )(implicit taCache: TypeAdapterCache, tt: TypeTag[T]): TypeAdapter[T] =
    if (tt.tpe.typeSymbol.fullName == "scala.Enumeration.Value") {
      // Can't use tpe <:< because Enumeration has no companion object
      val erasedEnumClassName = tt.tpe.toString match {
        case raw if raw.endsWith(".Value") => raw.replace(".Value", "$")
        case raw                           => raw.dropRight(raw.length - raw.lastIndexOf('.')) + "$"
      }
      val enum = Class
        .forName(erasedEnumClassName)
        .getField(scala.reflect.NameTransformer.MODULE_INSTANCE_NAME)
        .get(null)
        .asInstanceOf[Enumeration]
      EnumerationTypeAdapter(enum, taCache.jackFlavor)
        .asInstanceOf[TypeAdapter[T]]
    } else {
      next.typeAdapterOf[T]
    }
}

case class EnumerationTypeAdapter[E <: Enumeration](
    enum:       E,
    jackFlavor: JackFlavor[_])
  extends TypeAdapter[E#Value] {

  def read(parser: Parser): E#Value =
    if (parser.nextIsString) {
      val e = parser.expectString()
      if (e == null)
        null
      else
        Try(enum.withName(e)) match {
          case Success(u) => u
          case Failure(u) =>
            parser.backspace()
            throw new ScalaJackError(
              parser.showError(
                s"No value found in enumeration ${enum.getClass.getName} for $e"
              )
            )
        }
    } else if (parser.nextIsNumber) {
      val en = parser.expectNumber()
      Try(enum(en.toInt)) match {
        case Success(u) => u
        case Failure(u) =>
          parser.backspace()
          throw new ScalaJackError(
            parser.showError(
              s"No value found in enumeration ${enum.getClass.getName} for $en"
            )
          )
      }
    } else
      throw new ScalaJackError(
        parser.showError(s"Expected a Number or String here")
      )

  def write[WIRE](
      t:      E#Value,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit =
    t match {
      case null                       => writer.writeNull(out)
      case v if jackFlavor.enumsAsInt => writer.writeInt(v.id, out)
      case v                          => writer.writeString(v.toString, out)
    }
}
