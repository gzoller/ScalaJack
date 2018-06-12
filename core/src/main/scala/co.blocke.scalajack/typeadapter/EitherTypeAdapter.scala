package co.blocke.scalajack.typeadapter

import co.blocke.scalajack.{ Context, Deserializer, Reader, Serializer, TypeAdapter, TypeAdapterFactory, Writer }

import scala.language.existentials
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.TypeTag
import scala.util.{ Failure, Success, Try }

object EitherTypeAdapter extends TypeAdapterFactory.=:=.withTwoTypeParams[Either] {

  override def create[L, R](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[Either[L, R]], ttLeft: TypeTag[L], ttRight: TypeTag[R]): TypeAdapter[Either[L, R]] = {
    val leftType = ttLeft.tpe
    val rightType = ttRight.tpe

    if (leftType <:< rightType || rightType <:< leftType) {
      throw new IllegalArgumentException(s"Types $leftType and $rightType are not mutually exclusive")
    }

    val leftTypeAdapter = context.typeAdapterOf[L]
    val rightTypeAdapter = context.typeAdapterOf[R]

    EitherTypeAdapter(
      new EitherDeserializer(leftTypeAdapter.deserializer, rightTypeAdapter.deserializer),
      new EitherSerializer(leftTypeAdapter.serializer, rightTypeAdapter.serializer),
      leftTypeAdapter,
      rightTypeAdapter)
  }

}

case class EitherTypeAdapter[L, R](override val deserializer: Deserializer[Either[L, R]], override val serializer: Serializer[Either[L, R]], leftTypeAdapter: TypeAdapter[L], rightTypeAdapter: TypeAdapter[R])(implicit ttLeft: TypeTag[L], ttRight: TypeTag[R]) extends TypeAdapter[Either[L, R]] {

  private val leftClass = currentMirror.runtimeClass(ttLeft.tpe)
  private val rightClass = currentMirror.runtimeClass(ttRight.tpe)

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
