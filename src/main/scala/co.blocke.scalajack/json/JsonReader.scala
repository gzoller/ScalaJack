package co.blocke.scalajack
package json

import co.blocke.scala_reflection.reflect.rtypeRefs.*
import co.blocke.scala_reflection.reflect.*
import co.blocke.scala_reflection.{Clazzes, RTypeRef, TypedName}
import co.blocke.scala_reflection.rtypes.*
import co.blocke.scala_reflection.Liftables.TypedNameToExpr
import scala.quoted.*
import scala.collection.Factory
import co.blocke.scala_reflection.RType
import scala.jdk.CollectionConverters.*
import java.util.concurrent.ConcurrentHashMap
import scala.util.{Failure, Success, Try}

object JsonReader:

  def classInstantiator[T: Type](ref: ClassRef[T])(using Quotes): Expr[Map[String, ?] => T] =
    import quotes.reflect.*
    val sym = TypeRepr.of[T].classSymbol.get
    '{ (fieldMap: Map[String, ?]) =>
      ${
        val tree = Apply(
          Select.unique(New(TypeIdent(sym)), "<init>"),
          ref.fields.map { f =>
            f.fieldRef.refType match
              case '[t] =>
                '{ fieldMap(${ Expr(f.name) }).asInstanceOf[t] }.asTerm
          }
        )
        tree.asExpr.asExprOf[T]
      }
    }

  def refFn[T: Type](ref: RTypeRef[T])(using Quotes): Expr[(JsonConfig, JsonParser) => Either[ParseError, T]] =
    import Clazzes.*
    import quotes.reflect.*

    ref match
      case t: PrimitiveRef[?] if t.name == BOOLEAN_CLASS =>
        '{ (j: JsonConfig, p: JsonParser) => p.expectBoolean(j, p).map(_.asInstanceOf[T]) }
      case t: PrimitiveRef[?] if t.name == BYTE_CLASS =>
        '{ (j: JsonConfig, p: JsonParser) => p.expectLong(j, p).map(_.toByte.asInstanceOf[T]) }
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
        '{ (j: JsonConfig, p: JsonParser) => p.expectDouble(j, p).map(_.asInstanceOf[T]) }
      case t: PrimitiveRef[?] if t.name == FLOAT_CLASS =>
        '{ (j: JsonConfig, p: JsonParser) => p.expectDouble(j, p).map(_.toFloat.asInstanceOf[T]) }
      case t: PrimitiveRef[?] if t.name == INT_CLASS =>
        '{ (j: JsonConfig, p: JsonParser) => p.expectLong(j, p).map(_.toInt.asInstanceOf[T]) }
      case t: PrimitiveRef[?] if t.name == LONG_CLASS =>
        '{ (j: JsonConfig, p: JsonParser) => p.expectLong(j, p).map(_.asInstanceOf[T]) }
      case t: PrimitiveRef[?] if t.name == SHORT_CLASS =>
        '{ (j: JsonConfig, p: JsonParser) => p.expectLong(j, p).map(_.toShort.asInstanceOf[T]) }
      case t: PrimitiveRef[T] if t.name == STRING_CLASS =>
        '{ (j: JsonConfig, p: JsonParser) => p.expectString(j, p).map(_.asInstanceOf[T]) }

      case t: SeqRef[T] =>
        t.refType match
          case '[s] =>
            t.elementRef.refType match
              case '[e] =>
                val subFn = refFn[e](t.elementRef.asInstanceOf[RTypeRef[e]]).asInstanceOf[Expr[(JsonConfig, JsonParser) => Either[ParseError, e]]]
                '{ (j: JsonConfig, p: JsonParser) =>
                  p.expectList[e](j, $subFn).map(_.to(${ Expr.summon[Factory[e, T]].get })) // Convert List to whatever the target type should be
                }
      case t: ArrayRef[T] =>
        t.refType match
          case '[s] =>
            t.elementRef.refType match
              case '[e] =>
                val subFn = refFn[e](t.elementRef.asInstanceOf[RTypeRef[e]]).asInstanceOf[Expr[(JsonConfig, JsonParser) => Either[ParseError, e]]]
                '{ (j: JsonConfig, p: JsonParser) =>
                  p.expectList[e](j, $subFn).map(_.to(${ Expr.summon[Factory[e, T]].get })) // Convert List to whatever the target type should be
                }

      case t: ScalaEnumRef[T] =>
        t.refType match
          case '[s] =>
            val rtypeExpr = t.expr
            '{ (j: JsonConfig, p: JsonParser) =>
              val rtype = $rtypeExpr.asInstanceOf[ScalaEnumRType[T]]
              j.enumsAsIds match
                case '*' =>
                  p.expectLong(j, p).flatMap { v =>
                    val fromOrdinalMethod = Class.forName(rtype.name).getMethod("fromOrdinal", classOf[Int])
                    scala.util.Try(fromOrdinalMethod.invoke(null, v.toInt).asInstanceOf[T]) match
                      case Success(v2) => Right(v2)
                      case Failure(e)  => Left(JsonParseError(s"No enum value in ${rtype.name} for ordinal value '$v'"))
                  }
                case enumList: List[String] if enumList.contains(rtype.name) =>
                  p.expectLong(j, p).flatMap { v =>
                    val fromOrdinalMethod = Class.forName(rtype.name).getMethod("fromOrdinal", classOf[Int])
                    scala.util.Try(fromOrdinalMethod.invoke(null, v.toInt).asInstanceOf[T]) match
                      case Success(v2) => Right(v2)
                      case Failure(e)  => Left(JsonParseError(s"No enum value in ${rtype.name} for ordinal value '$v'"))
                  }
                case _ =>
                  p.expectString(j, p).flatMap { v =>
                    val valueOfMethod = Class.forName(rtype.name).getMethod("valueOf", classOf[String])
                    scala.util.Try(valueOfMethod.invoke(null, v).asInstanceOf[T]) match
                      case Success(v2) => Right(v2)
                      case Failure(e)  => Left(JsonParseError(s"No enum value in ${rtype.name} for value '$v'"))
                  }
            }

  def classParseMap[T: Type](ref: ClassRef[T])(using Quotes): Expr[JsonParser => Map[String, (JsonConfig, JsonParser) => Either[ParseError, ?]]] =
    import Clazzes.*
    '{ (parser: JsonParser) =>
      val daList = ${
        val fieldList = ref.fields.map(f =>
          f.fieldRef.refType match
            case '[m] =>
              val fn = refFn[m](f.fieldRef.asInstanceOf[RTypeRef[m]])
              '{
                ${ Expr(f.name) } -> $fn
              }
        )
        Expr.ofList(fieldList)
      }
      daList.toMap
    }
