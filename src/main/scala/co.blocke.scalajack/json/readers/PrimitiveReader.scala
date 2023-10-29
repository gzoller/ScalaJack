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

      //
      // Scala Primitives
      //
      case t: PrimitiveRef[T] if t.name == BIG_DECIMAL_CLASS =>
        if isMapKey then
          '{ (j: JsonConfig, p: JsonParser) =>
            (for {
              _ <- p.expectQuote
              v <- p.expectBigDouble(j, p).map(_.asInstanceOf[T])
              _ <- p.expectQuote
            } yield v)
          }
        else '{ (j: JsonConfig, p: JsonParser) => p.expectBigDouble(j, p).map(_.asInstanceOf[T]) }

      case t: PrimitiveRef[T] if t.name == BIG_INT_CLASS =>
        if isMapKey then
          '{ (j: JsonConfig, p: JsonParser) =>
            (for {
              _ <- p.expectQuote
              v <- p.expectBigLong(j, p).map(_.asInstanceOf[T])
              _ <- p.expectQuote
            } yield v)
          }
        else '{ (j: JsonConfig, p: JsonParser) => p.expectBigLong(j, p).map(_.asInstanceOf[T]) }

      case t: PrimitiveRef[T] if t.name == BOOLEAN_CLASS =>
        if isMapKey then
          '{ (j: JsonConfig, p: JsonParser) =>
            (for {
              _ <- p.expectQuote
              v <- p.expectBoolean(j, p).map(_.asInstanceOf[T])
              _ <- p.expectQuote
            } yield v)
          }
        else '{ (j: JsonConfig, p: JsonParser) => p.expectBoolean(j, p).map(_.asInstanceOf[T]) }

      case t: PrimitiveRef[T] if t.name == BYTE_CLASS =>
        if isMapKey then
          '{ (j: JsonConfig, p: JsonParser) =>
            (for {
              _ <- p.expectQuote
              v <- p.expectLong(j, p).map(_.toByte.asInstanceOf[T])
              _ <- p.expectQuote
            } yield v)
          }
        else '{ (j: JsonConfig, p: JsonParser) => p.expectLong(j, p).map(_.toByte.asInstanceOf[T]) }

      case t: PrimitiveRef[T] if t.name == CHAR_CLASS =>
        '{ (j: JsonConfig, p: JsonParser) =>
          p.expectString(j, p)
            .flatMap(s =>
              s.toArray.headOption match
                case Some(c) => Right(c.asInstanceOf[T])
                case None    => Left(JsonParseError(p.showError(s"Cannot convert value '$s' into a Char.")))
            )
        }

      case t: PrimitiveRef[T] if t.name == DOUBLE_CLASS =>
        if isMapKey then
          '{ (j: JsonConfig, p: JsonParser) =>
            (for {
              _ <- p.expectQuote
              v <- p.expectDouble(j, p).map(_.asInstanceOf[T])
              _ <- p.expectQuote
            } yield v)
          }
        else '{ (j: JsonConfig, p: JsonParser) => p.expectDouble(j, p).map(_.asInstanceOf[T]) }

      case t: PrimitiveRef[T] if t.name == FLOAT_CLASS =>
        if isMapKey then
          '{ (j: JsonConfig, p: JsonParser) =>
            (for {
              _ <- p.expectQuote
              v <- p.expectDouble(j, p).map(_.toFloat.asInstanceOf[T])
              _ <- p.expectQuote
            } yield v)
          }
        else '{ (j: JsonConfig, p: JsonParser) => p.expectDouble(j, p).map(_.toFloat.asInstanceOf[T]) }

      case t: PrimitiveRef[T] if t.name == INT_CLASS =>
        if isMapKey then
          '{ (j: JsonConfig, p: JsonParser) =>
            (for {
              _ <- p.expectQuote
              v <- p.expectLong(j, p).map(_.toInt.asInstanceOf[T])
              _ <- p.expectQuote
            } yield v)
          }
        else '{ (j: JsonConfig, p: JsonParser) => p.expectLong(j, p).map(_.toInt.asInstanceOf[T]) }

      case t: PrimitiveRef[T] if t.name == LONG_CLASS =>
        if isMapKey then
          '{ (j: JsonConfig, p: JsonParser) =>
            (for {
              _ <- p.expectQuote
              v <- p.expectLong(j, p).map(_.asInstanceOf[T])
              _ <- p.expectQuote
            } yield v)
          }
        else '{ (j: JsonConfig, p: JsonParser) => p.expectLong(j, p).map(_.asInstanceOf[T]) }

      case t: PrimitiveRef[T] if t.name == SHORT_CLASS =>
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

      //
      // Java Primitives
      //
      case t: PrimitiveRef[T] if t.name == JBOOLEAN_CLASS =>
        if isMapKey then
          '{ (j: JsonConfig, p: JsonParser) =>
            (for {
              _ <- p.expectQuote
              v <- p.expectBoolean(j, p).map(b => java.lang.Boolean(b).asInstanceOf[T])
              _ <- p.expectQuote
            } yield v)
          }
        else '{ (j: JsonConfig, p: JsonParser) => p.expectBoolean(j, p).map(_.asInstanceOf[T]) }

      case t: PrimitiveRef[T] if t.name == JBYTE_CLASS =>
        if isMapKey then
          '{ (j: JsonConfig, p: JsonParser) =>
            (for {
              _ <- p.expectQuote
              v <- p.expectLong(j, p).map(b => java.lang.Byte(b.asInstanceOf[Byte]).asInstanceOf[T])
              _ <- p.expectQuote
            } yield v)
          }
        else '{ (j: JsonConfig, p: JsonParser) => p.expectLong(j, p).map(_.toByte.asInstanceOf[T]) }

      case t: PrimitiveRef[T] if t.name == JCHARACTER_CLASS =>
        '{ (j: JsonConfig, p: JsonParser) =>
          p.expectString(j, p)
            .flatMap(s =>
              s.toArray.headOption match
                case Some(c) => Right(java.lang.Character(c).asInstanceOf[T])
                case None    => Left(JsonParseError(p.showError(s"Cannot convert value '$s' into a Char.")))
            )
        }

      case t: PrimitiveRef[T] if t.name == JDOUBLE_CLASS =>
        if isMapKey then
          '{ (j: JsonConfig, p: JsonParser) =>
            (for {
              _ <- p.expectQuote
              v <- p.expectDouble(j, p).map(b => java.lang.Double(b).asInstanceOf[T])
              _ <- p.expectQuote
            } yield v)
          }
        else '{ (j: JsonConfig, p: JsonParser) => p.expectDouble(j, p).map(_.asInstanceOf[T]) }

      case t: PrimitiveRef[T] if t.name == JFLOAT_CLASS =>
        if isMapKey then
          '{ (j: JsonConfig, p: JsonParser) =>
            (for {
              _ <- p.expectQuote
              v <- p.expectDouble(j, p).map(b => java.lang.Float(b).asInstanceOf[T])
              _ <- p.expectQuote
            } yield v)
          }
        else '{ (j: JsonConfig, p: JsonParser) => p.expectDouble(j, p).map(_.toFloat.asInstanceOf[T]) }

      case t: PrimitiveRef[T] if t.name == JINTEGER_CLASS =>
        if isMapKey then
          '{ (j: JsonConfig, p: JsonParser) =>
            (for {
              _ <- p.expectQuote
              v <- p.expectLong(j, p).map(b => java.lang.Integer(b.toInt).asInstanceOf[T])
              _ <- p.expectQuote
            } yield v)
          }
        else '{ (j: JsonConfig, p: JsonParser) => p.expectLong(j, p).map(_.toInt.asInstanceOf[T]) }

      case t: PrimitiveRef[T] if t.name == JLONG_CLASS =>
        if isMapKey then
          '{ (j: JsonConfig, p: JsonParser) =>
            (for {
              _ <- p.expectQuote
              v <- p.expectLong(j, p).map(b => java.lang.Long(b).asInstanceOf[T])
              _ <- p.expectQuote
            } yield v)
          }
        else '{ (j: JsonConfig, p: JsonParser) => p.expectLong(j, p).map(_.asInstanceOf[T]) }

      case t: PrimitiveRef[T] if t.name == JSHORT_CLASS =>
        if isMapKey then
          '{ (j: JsonConfig, p: JsonParser) =>
            (for {
              _ <- p.expectQuote
              v <- p.expectLong(j, p).map(b => java.lang.Short(b.toShort).asInstanceOf[T])
              _ <- p.expectQuote
            } yield v)
          }
        else '{ (j: JsonConfig, p: JsonParser) => p.expectLong(j, p).map(_.toShort.asInstanceOf[T]) }

      case t: PrimitiveRef[T] if t.name == UUID_CLASS =>
        '{ (j: JsonConfig, p: JsonParser) =>
          p.expectString(j, p)
            .flatMap(u =>
              if u == null then Right(null.asInstanceOf[T])
              else
                scala.util.Try(java.util.UUID.fromString(u)) match
                  case Success(uuid) => Right(uuid.asInstanceOf[T])
                  case Failure(_)    => Left(JsonParseError(p.showError(s"Unable to marshal UUID from value '$u'.")))
            )
        }

      case t =>
        next.readerFn[T](t)
