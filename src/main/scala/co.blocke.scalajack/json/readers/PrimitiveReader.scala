package co.blocke.scalajack
package json
package readers

import co.blocke.scala_reflection.reflect.rtypeRefs.*
import co.blocke.scala_reflection.reflect.*
import co.blocke.scala_reflection.{Clazzes, RTypeRef, TypedName}
import co.blocke.scala_reflection.rtypes.*
import co.blocke.scala_reflection.Liftables.TypedNameToExpr
import scala.quoted.*
import scala.collection.Factory
import co.blocke.scala_reflection.RType
import scala.jdk.CollectionConverters.*
import scala.collection.mutable.HashMap
import scala.util.{Failure, Success, Try}

case class PrimitiveReader(next: ReaderModule, root: ReaderModule) extends ReaderModule:

  def readerFn[T](ref: RTypeRef[T], isMapKey: Boolean = false)(using q: Quotes, tt: Type[T])(using cache: HashMap[Expr[TypedName], Expr[(JsonConfig, JsonParser) => Either[ParseError, ?]]]): Expr[(JsonConfig, JsonParser) => Either[ParseError, T]] =
    import Clazzes.*
    import quotes.reflect.*

    ref match
      case t: PrimitiveRef[?] if t.name == BOOLEAN_CLASS =>
        if isMapKey then
          '{ (j: JsonConfig, p: JsonParser) =>
            (for {
              _ <- p.expectQuote
              v <- p.expectBoolean(j, p).map(_.asInstanceOf[T])
              _ <- p.expectQuote
            } yield v)
          }
        else '{ (j: JsonConfig, p: JsonParser) => p.expectBoolean(j, p).map(_.asInstanceOf[T]) }
      case t: PrimitiveRef[?] if t.name == BYTE_CLASS =>
        if isMapKey then
          '{ (j: JsonConfig, p: JsonParser) =>
            (for {
              _ <- p.expectQuote
              v <- p.expectLong(j, p).map(_.toByte.asInstanceOf[T])
              _ <- p.expectQuote
            } yield v)
          }
        else '{ (j: JsonConfig, p: JsonParser) => p.expectLong(j, p).map(_.toByte.asInstanceOf[T]) }
      case t: PrimitiveRef[?] if t.name == CHAR_CLASS =>
        '{ (j: JsonConfig, p: JsonParser) =>
          p.expectString(j, p)
            .flatMap(s =>
              s.toArray.headOption match
                case Some(c) => Right(c.asInstanceOf[T])
                case None    => Left(JsonParseError(s"Cannot convert value '$s' into a Char."))
            )
        }
      case t: PrimitiveRef[?] if t.name == DOUBLE_CLASS =>
        if isMapKey then
          '{ (j: JsonConfig, p: JsonParser) =>
            (for {
              _ <- p.expectQuote
              v <- p.expectDouble(j, p).map(_.asInstanceOf[T])
              _ <- p.expectQuote
            } yield v)
          }
        else '{ (j: JsonConfig, p: JsonParser) => p.expectDouble(j, p).map(_.asInstanceOf[T]) }
      case t: PrimitiveRef[?] if t.name == FLOAT_CLASS =>
        if isMapKey then
          '{ (j: JsonConfig, p: JsonParser) =>
            (for {
              _ <- p.expectQuote
              v <- p.expectDouble(j, p).map(_.toFloat.asInstanceOf[T])
              _ <- p.expectQuote
            } yield v)
          }
        else '{ (j: JsonConfig, p: JsonParser) => p.expectDouble(j, p).map(_.toFloat.asInstanceOf[T]) }
      case t: PrimitiveRef[?] if t.name == INT_CLASS =>
        if isMapKey then
          '{ (j: JsonConfig, p: JsonParser) =>
            (for {
              _ <- p.expectQuote
              v <- p.expectLong(j, p).map(_.toInt.asInstanceOf[T])
              _ <- p.expectQuote
            } yield v)
          }
        else '{ (j: JsonConfig, p: JsonParser) => p.expectLong(j, p).map(_.toInt.asInstanceOf[T]) }
      case t: PrimitiveRef[?] if t.name == LONG_CLASS =>
        if isMapKey then
          '{ (j: JsonConfig, p: JsonParser) =>
            (for {
              _ <- p.expectQuote
              v <- p.expectLong(j, p).map(_.asInstanceOf[T])
              _ <- p.expectQuote
            } yield v)
          }
        else '{ (j: JsonConfig, p: JsonParser) => p.expectLong(j, p).map(_.asInstanceOf[T]) }
      case t: PrimitiveRef[?] if t.name == SHORT_CLASS =>
        if isMapKey then
          '{ (j: JsonConfig, p: JsonParser) =>
            (for {
              _ <- p.expectQuote
              v <- p.expectLong(j, p).map(_.toShort.asInstanceOf[T])
              _ <- p.expectQuote
            } yield v)
          }
        else '{ (j: JsonConfig, p: JsonParser) => p.expectLong(j, p).map(_.toShort.asInstanceOf[T]) }
      case t: PrimitiveRef[T] if t.name == STRING_CLASS =>
        '{ (j: JsonConfig, p: JsonParser) => p.expectString(j, p).map(_.asInstanceOf[T]) }

      case t => 
        next.readerFn[T](t)