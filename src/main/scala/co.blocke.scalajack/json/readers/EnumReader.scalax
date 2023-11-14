package co.blocke.scalajack
package json
package readers

import co.blocke.scala_reflection.reflect.rtypeRefs.*
import co.blocke.scala_reflection.rtypes.*
import co.blocke.scala_reflection.{Clazzes, RTypeRef, TypedName}
import scala.quoted.*
import scala.collection.mutable.HashMap
import scala.util.{Failure, Success, Try}

case class EnumReader(next: ReaderModule, root: ReaderModule) extends ReaderModule:

  def readerFn[T](ref: RTypeRef[T], isMapKey: Boolean = false)(using q: Quotes, tt: Type[T])(using cache: HashMap[Expr[TypedName], Expr[(JsonConfig, JsonParser) => Either[ParseError, ?]]]): Expr[(JsonConfig, JsonParser) => Either[ParseError, T]] =
    import quotes.reflect.*
    import Clazzes.*

    ref match
      case t: ScalaEnumRef[T] =>
        t.refType match
          case '[s] =>
            val rtypeExpr = t.expr
            val wrappedMapKey = Expr(isMapKey)
            '{ (j: JsonConfig, p: JsonParser) =>
              val rtype = $rtypeExpr.asInstanceOf[ScalaEnumRType[T]]
              def readNumericEnum =
                if $wrappedMapKey then
                  (for {
                    _ <- p.expectQuote
                    enumVal <- p.expectLong(j, p)
                    _ <- p.expectQuote
                  } yield enumVal).flatMap { v =>
                    val fromOrdinalMethod = Class.forName(rtype.name).getMethod("fromOrdinal", classOf[Int])
                    scala.util.Try(fromOrdinalMethod.invoke(null, v.toInt).asInstanceOf[T]) match
                      case Success(v2) => Right(v2)
                      case Failure(e)  => Left(JsonParseError(p.showError(s"No enum value in ${rtype.name} for ordinal value '$v'")))
                  }
                else
                  p.expectLong(j, p).flatMap { v =>
                    val fromOrdinalMethod = Class.forName(rtype.name).getMethod("fromOrdinal", classOf[Int])
                    scala.util.Try(fromOrdinalMethod.invoke(null, v.toInt).asInstanceOf[T]) match
                      case Success(v2) => Right(v2)
                      case Failure(e)  => Left(JsonParseError(p.showError(s"No enum value in ${rtype.name} for ordinal value '$v'")))
                  }
              j.enumsAsIds match
                case '*'                                                     => readNumericEnum
                case enumList: List[String] if enumList.contains(rtype.name) => readNumericEnum
                case _ =>
                  p.expectString(j, p).flatMap { v =>
                    val valueOfMethod = Class.forName(rtype.name).getMethod("valueOf", classOf[String])
                    scala.util.Try(valueOfMethod.invoke(null, v).asInstanceOf[T]) match
                      case Success(v2) => Right(v2)
                      case Failure(e)  => Left(JsonParseError(p.showError(s"No enum value in ${rtype.name} for value '$v'")))
                  }
            }

      case t: ScalaEnumerationRef[T] =>
        t.refType match
          case '[s] =>
            val rtypeExpr = t.expr
            val wrappedMapKey = Expr(isMapKey)
            '{ (j: JsonConfig, p: JsonParser) =>
              val rtype = $rtypeExpr.asInstanceOf[ScalaEnumerationRType[T]]
              def readNumericEnum =
                val eClazz = Class.forName(rtype.name)
                val fromOrdinalMethod = eClazz.getMethod("apply", classOf[Int])
                if $wrappedMapKey then
                  (for {
                    _ <- p.expectQuote
                    enumVal <- p.expectLong(j, p)
                    _ <- p.expectQuote
                  } yield enumVal).flatMap { v =>
                    scala.util.Try(fromOrdinalMethod.invoke(null, v.toInt).asInstanceOf[T]) match
                      case Success(v2) => Right(v2)
                      case Failure(e)  => Left(JsonParseError(p.showError(s"No enumeration value in ${rtype.name} for ordinal value '$v'")))
                  }
                else
                  p.expectLong(j, p).flatMap { v =>
                    scala.util.Try(fromOrdinalMethod.invoke(null, v.toInt).asInstanceOf[T]) match
                      case Success(v2) => Right(v2)
                      case Failure(e)  => Left(JsonParseError(p.showError(s"No enumeration value in ${rtype.name} for ordinal value '$v'")))
                  }
              j.enumsAsIds match
                case '*'                                                     => readNumericEnum
                case enumList: List[String] if enumList.contains(rtype.name) => readNumericEnum
                case _ =>
                  p.expectString(j, p).flatMap { v =>
                    val eClazz = Class.forName(rtype.name)
                    val valueOfMethod = eClazz.getMethod("withName", classOf[String])
                    scala.util.Try(valueOfMethod.invoke(null, v).asInstanceOf[T]) match
                      case Success(v2) => Right(v2)
                      case Failure(e)  => Left(JsonParseError(p.showError(s"No enumeration value in ${rtype.name} for value '$v'")))
                  }
            }

      // TODO: JavaEnumRef

      case t =>
        next.readerFn[T](t)
