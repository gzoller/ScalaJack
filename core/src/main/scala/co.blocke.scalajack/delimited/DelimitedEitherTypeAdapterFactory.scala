package co.blocke.scalajack
package delimited

import model._

import scala.collection.mutable
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{ NoType, Type, TypeTag, typeOf }
import scala.util.{ Failure, Success, Try }

/**
 * The only reason this machinery exists for delimited either is that writes need to be string-wrapped, where they don't in the general/JSON case.
 */
object DelimitedEitherTypeAdapterFactory extends TypeAdapterFactory {

  // $COVERAGE-OFF$All this is carbon-copy from EitherTypeAdapterFactory, which has test coverage.
  override def typeAdapterOf[T](
      next: TypeAdapterFactory
  )(implicit taCache: TypeAdapterCache, tt: TypeTag[T]): TypeAdapter[T] =
    tt.tpe.baseType(typeOf[Either[_, _]].typeSymbol) match {
      case NoType =>
        next.typeAdapterOf[T]

      case asEither =>
        val leftType :: rightType :: Nil = asEither.typeArgs

        if (leftType <:< rightType || rightType <:< leftType) {
          throw new IllegalArgumentException(
            s"Types $leftType and $rightType are not mutually exclusive"
          )
        }

        val leftTypeAdapter = taCache.typeAdapter(leftType)
        val rightTypeAdapter = taCache.typeAdapter(rightType)
        DelimitedEitherTypeAdapter(
          leftTypeAdapter,
          rightTypeAdapter,
          leftType,
          rightType
        ).asInstanceOf[TypeAdapter[T]]
    }
  // $COVERAGE-ON$

}

case class DelimitedEitherTypeAdapter[L, R](leftTypeAdapter: TypeAdapter[L], rightTypeAdapter: TypeAdapter[R], leftType: Type, rightType: Type)
  extends TypeAdapter[Either[L, R]] {

  val leftClass: Class[_] = currentMirror.runtimeClass(leftType)
  val rightClass: Class[_] = currentMirror.runtimeClass(rightType)

  def read(parser: Parser): Either[L, R] = {
    val savedReader = parser.mark()
    if (parser.peekForNull)
      null
    else
      tryRead(rightTypeAdapter, parser) match {
        case Success(rightValue) =>
          Right(rightValue.asInstanceOf[R])
        case Failure(_) => // Right parse failed... try left
          parser.revertToMark(savedReader)
          tryRead(leftTypeAdapter, parser) match {
            case Success(leftValue) =>
              Left(leftValue.asInstanceOf[L])
            case Failure(x) =>
              parser.backspace()
              throw new ScalaJackError(
                parser.showError(s"Failed to read either side of Either")
              )
          }
      }
  }

  private def tryRead(ta: TypeAdapter[_], parser: Parser): Try[_] =
    ta match {
      case _: Classish =>
        val subParser = parser.subParser(parser.expectString().asInstanceOf[parser.WIRE])
        Try(ta.read(subParser))
      case _ =>
        Try(ta.read(parser))
    }

  def write[WIRE](t: Either[L, R], writer: Writer[WIRE], out: mutable.Builder[WIRE, WIRE]): Unit =
    t match {
      case null => writer.writeNull(out)
      case Left(v) =>
        leftTypeAdapter match {
          case lta: Classish =>
            val sb = compat.StringBuilder()
            lta.write(v, writer.asInstanceOf[Writer[String]], sb)
            writer.writeString(sb.result(), out)
          case _ => leftTypeAdapter.write(v, writer, out)
        }
      case Right(v) =>
        rightTypeAdapter match {
          case rta: Classish =>
            val sb = compat.StringBuilder()
            rta.write(v, writer.asInstanceOf[Writer[String]], sb)
            writer.writeString(sb.result, out)
          case _ => rightTypeAdapter.write(v, writer, out)
        }
    }
}
