package co.blocke.series5.typeadapter

import co.blocke.series5.{ Context, Reader, TypeAdapter, TypeAdapterFactory, Writer }

import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{ NoType, Type, TypeTag, typeOf }
import scala.util.{ Try, Success, Failure }

object EitherTypeAdapter extends TypeAdapterFactory {

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

  override def read(reader: Reader): Either[L, R] = {
    val savePos = reader.position // in case we need to re-parse as Left
    Try(rightTypeAdapter.read(reader)) match {
      case Success(rightValue) =>
        if (leftClass.isInstance(rightValue)) {
          // $COVERAGE-OFF$Nice safety check but logically not possible to get here due to check in factory
          throw new RuntimeException(s"$rightValue (of ${rightValue.getClass}) is an instance of both $leftClass and $rightClass.\n" + reader.showError())
          // $COVERAGE-ON$
        } else {
          Right(rightValue.asInstanceOf[R])
        }
      case Failure(_) => // Right parse failed... try left
        reader.position = savePos
        Try(leftTypeAdapter.read(reader)) match {
          case Success(leftValue) =>
            if (rightClass.isInstance(leftValue)) {
              // $COVERAGE-OFF$Nice safety check but logically not possible to get here due to check in factory
              throw new RuntimeException(s"$leftValue (of ${leftValue.getClass}) is an instance of both $leftClass and $rightClass.\n" + reader.showError())
              // $COVERAGE-ON$
            } else {
              Left(leftValue.asInstanceOf[L])
            }
          case Failure(_) =>
            throw new RuntimeException(s"Parsed value fits neither class ${leftClass} nor ${rightClass}\n" + reader.showError())
        }
    }
  }

  override def write(either: Either[L, R], writer: Writer): Unit =
    either match {
      case null =>
        writer.writeNull()

      case Left(value) =>
        leftTypeAdapter.write(value, writer)

      case Right(value) =>
        rightTypeAdapter.write(value, writer)
    }

}
