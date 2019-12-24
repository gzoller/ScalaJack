package co.blocke.scalajack
package typeadapter

import model._

import scala.collection.mutable.Builder
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{ NoType, Type, TypeTag, typeOf }
import scala.util.{ Failure, Success, Try }

object EitherTypeAdapterFactory extends TypeAdapterFactory {

  override def typeAdapterOf[T](
      next: TypeAdapterFactory
  )(implicit taCache: TypeAdapterCache, tt: TypeTag[T]): TypeAdapter[T] =
    tt.tpe.baseType(typeOf[Either[_, _]].typeSymbol) match {
      case NoType =>
        next.typeAdapterOf[T]

      case asEither =>
        val List(leftType, rightType) = asEither.typeArgs.take(2)

        if (leftType <:< rightType || rightType <:< leftType) {
          throw new IllegalArgumentException(
            s"Types $leftType and $rightType are not mutually exclusive"
          )
        }

        val leftTypeAdapter = taCache.typeAdapter(leftType)
        val rightTypeAdapter = taCache.typeAdapter(rightType)
        EitherTypeAdapter(
          leftTypeAdapter,
          rightTypeAdapter,
          leftType,
          rightType
        ).asInstanceOf[TypeAdapter[T]]
    }

}

case class EitherTypeAdapter[L, R](
    leftTypeAdapter:  TypeAdapter[L],
    rightTypeAdapter: TypeAdapter[R],
    leftType:         Type,
    rightType:        Type)
  extends TypeAdapter[Either[L, R]] {

  val leftClass: Class[_] = currentMirror.runtimeClass(leftType)
  val rightClass: Class[_] = currentMirror.runtimeClass(rightType)

  def read(parser: Parser): Either[L, R] = {
    val savedReader = parser.mark()
    if (parser.peekForNull)
      null
    else
      Try(rightTypeAdapter.read(parser)) match {
        case Success(rightValue) =>
          Right(rightValue.asInstanceOf[R])
        case Failure(_) => // Right parse failed... try left
          parser.revertToMark(savedReader)
          Try(leftTypeAdapter.read(parser)) match {
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

  def write[WIRE](
      t:      Either[L, R],
      writer: Writer[WIRE],
      out:    Builder[WIRE, WIRE]): Unit =
    t match {
      case null     => writer.writeNull(out)
      case Left(v)  => leftTypeAdapter.write(v, writer, out)
      case Right(v) => rightTypeAdapter.write(v, writer, out)
    }
}
