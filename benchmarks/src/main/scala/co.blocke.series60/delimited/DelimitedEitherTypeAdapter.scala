package co.blocke.series60
package delimited

import compat.StringBuilder
import util.Path
import model._

import scala.collection.mutable.Builder
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{ NoType, Type, TypeTag, typeOf }
import scala.util.{ Failure, Success, Try }

// $COVERAGE-OFF$Exactly the same as EnumerationTypeAdapterFactory--don't need to test twice!
object DelimitedEitherTypeAdapterFactory extends TypeAdapterFactory {

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
        DelimitedEitherTypeAdapter(leftTypeAdapter, rightTypeAdapter, leftType, rightType).asInstanceOf[TypeAdapter[T]]
    }
}
// $COVERAGE-ON$

case class DelimitedEitherTypeAdapter[L, R](leftTypeAdapter: TypeAdapter[L], rightTypeAdapter: TypeAdapter[R], leftType: Type, rightType: Type) extends TypeAdapter[Either[L, R]] {

  val leftClass = currentMirror.runtimeClass(leftType)
  val rightClass = currentMirror.runtimeClass(rightType)

  def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): Either[L, R] = {
    val savedReader = reader.copy
    tryRead(path, reader, rightTypeAdapter, isMapKey) match {
      case Success(rightValue) =>
        Right(rightValue.asInstanceOf[R])
      case Failure(_) => // Right parse failed... try left
        reader.syncPositionTo(savedReader)
        tryRead(path, reader, leftTypeAdapter, isMapKey) match {
          case Success(leftValue) =>
            Left(leftValue.asInstanceOf[L])
          case Failure(x) =>
            reader.back
            throw new ReadMalformedError(reader.showError(path, s"Failed to read either side of Either"))
        }
    }
  }

  private def tryRead[WIRE](path: Path, reader: Reader[WIRE], ta: TypeAdapter[_], isMapKey: Boolean): Try[_] =
    reader.head match {
      case token if token.tokenType == TokenType.QuotedString && (ta.isInstanceOf[Collectionish] || ta.isInstanceOf[Classish]) =>
        Try(ta.read(path, reader.jackFlavor.parse(reader.readString(path).asInstanceOf[WIRE]), isMapKey))
      case _ =>
        Try(ta.read(path, reader, isMapKey))
    }

  def write[WIRE](t: Either[L, R], writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit =
    t match {
      case null     => writer.writeNull(out)
      case Left(v)  => tryWrite(v, writer, out, isMapKey, leftTypeAdapter)
      case Right(v) => tryWrite(v, writer, out, isMapKey, rightTypeAdapter)
    }

  private def tryWrite[WIRE, K](t: K, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean, ta: TypeAdapter[K]): Unit =
    if (ta.isInstanceOf[Classish]) {
      val sb = new StringBuilder().asInstanceOf[Builder[Any, Any]]
      ta.write(t, writer.asInstanceOf[Writer[Any]], sb, false)
      writer.writeString(sb.result().asInstanceOf[String], out) // wrap output in quotes/escape
    } else
      ta.write(t, writer, out, isMapKey)
}
