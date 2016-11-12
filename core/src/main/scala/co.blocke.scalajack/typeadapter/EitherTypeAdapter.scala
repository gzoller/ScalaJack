package co.blocke.scalajack.typeadapter

import co.blocke.scalajack.{ Context, Reader, TypeAdapter, TypeAdapterFactory, Writer }

import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{ NoType, Type, TypeTag, typeOf }

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

        val anyTypeAdapter = context.typeAdapterOf[Any]
        EitherTypeAdapter(anyTypeAdapter, leftType, rightType).asInstanceOf[TypeAdapter[T]]
    }

}

case class EitherTypeAdapter[L, R](anyTypeAdapter: TypeAdapter[Any], leftType: Type, rightType: Type) extends TypeAdapter[Either[L, R]] {

  val leftClass = currentMirror.runtimeClass(leftType)
  val rightClass = currentMirror.runtimeClass(rightType)

  override def read(reader: Reader): Either[L, R] = {
    val value = anyTypeAdapter.read(reader)

    if (leftClass.isInstance(value)) {
      if (rightClass.isInstance(value)) {
        throw new RuntimeException(s"$value (of ${value.getClass}) is an instance of both $leftClass and $rightClass!!!")
      } else {
        Left(value.asInstanceOf[L])
      }
    } else {
      if (rightClass.isInstance(value)) {
        Right(value.asInstanceOf[R])
      } else {
        throw new RuntimeException(s"$value (of ${value.getClass}) is neither an instance of $leftClass nor of $rightClass!!!")
      }
    }
  }

  override def write(either: Either[L, R], writer: Writer): Unit =
    either match {
      case null =>
        writer.writeNull()

      case Left(value) =>
        anyTypeAdapter.write(value, writer)

      case Right(value) =>
        anyTypeAdapter.write(value, writer)
    }

}
