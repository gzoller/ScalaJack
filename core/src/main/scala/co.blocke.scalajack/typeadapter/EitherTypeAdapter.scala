package co.blocke.scalajack
package typeadapter

import util.Path
import model._

import scala.collection.mutable.Builder
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{ NoType, Type, TypeTag, typeOf }
import scala.util.{ Failure, Success, Try }

object EitherTypeAdapterFactory extends TypeAdapterFactory {

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    tt.tpe.baseType(typeOf[Either[_, _]].typeSymbol) match {
      case NoType =>
        next.typeAdapterOf[T]

      case asEither =>
        val leftType :: rightType :: Nil = asEither.typeArgs

        if (leftType <:< rightType || rightType <:< leftType) {
          throw new IllegalArgumentException(s"Types $leftType and $rightType are not mutually exclusive")
        }

        val leftTypeAdapter = context.typeAdapter(leftType)
        val rightTypeAdapter = context.typeAdapter(rightType)
        EitherTypeAdapter(leftTypeAdapter, rightTypeAdapter, leftType, rightType).asInstanceOf[TypeAdapter[T]]
    }

}

case class EitherTypeAdapter[L, R](leftTypeAdapter: TypeAdapter[L], rightTypeAdapter: TypeAdapter[R], leftType: Type, rightType: Type) extends TypeAdapter[Either[L, R]] {

  val leftClass = currentMirror.runtimeClass(leftType)
  val rightClass = currentMirror.runtimeClass(rightType)

  def read[WIRE](path: Path, reader: Transceiver[WIRE]): Either[L, R] = {
    reader.savePos()
    reader.peek() match {
      case TokenType.Null =>
        reader.skip()
        null
      case v =>
        Try(rightTypeAdapter.read(path, reader)) match {
          case Success(rightValue) =>
            Right(rightValue.asInstanceOf[R])
          case Failure(_) => // Right parse failed... try left
            reader.rollbackToSave()
            Try(leftTypeAdapter.read(path, reader)) match {
              case Success(leftValue) =>
                Left(leftValue.asInstanceOf[L])
              case Failure(x) =>
                throw new ReadMalformedError(path, s"Failed to read either side of Either\n" + reader.showError(1), List.empty[String], x)
            }
        }
    }
  }

  def write[WIRE](t: Either[L, R], writer: Transceiver[WIRE], out: Builder[Any, WIRE], isMapKey: Boolean): Unit =
    t match {
      case null     => writer.writeNull(out)
      case Left(v)  => leftTypeAdapter.write(v, writer, out, isMapKey)
      case Right(v) => rightTypeAdapter.write(v, writer, out, isMapKey)
    }
}
