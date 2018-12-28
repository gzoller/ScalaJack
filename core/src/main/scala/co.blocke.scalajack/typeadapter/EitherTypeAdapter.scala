package co.blocke.scalajack
package typeadapter

import util.Path
import model._

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

  def read(path: Path, reader: Reader, isMapKey: Boolean): Either[L, R] = {
    reader.savePos()
    Try(rightTypeAdapter.read(path, reader, isMapKey)) match {
      case Success(rightValue) =>
        Right(rightValue.asInstanceOf[R])
      case Failure(_) => // Right parse failed... try left
        reader.rollbackToSave()
        Try(leftTypeAdapter.read(path, reader, isMapKey)) match {
          case Success(leftValue) =>
            Left(leftValue.asInstanceOf[L])
          case Failure(x) =>
            throw new SJReadError(path, Invalid, s"Failed to read either side of Either", List.empty[String], Some(x))
        }
    }
  }
}

/*
  override def write(either: Either[L, R], writer: Writer): Unit =
    either match {
      case null =>
        writer.writeNull()

      case Left(value) =>
        leftTypeAdapter.write(value, writer)

      case Right(value) =>
        rightTypeAdapter.write(value, writer)
    }
    */
