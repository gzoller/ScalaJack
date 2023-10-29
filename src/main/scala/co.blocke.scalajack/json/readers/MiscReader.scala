package co.blocke.scalajack
package json
package readers

import co.blocke.scala_reflection.reflect.rtypeRefs.*
import co.blocke.scala_reflection.rtypes.*
import co.blocke.scala_reflection.{Clazzes, RTypeRef, TypedName}
import scala.quoted.*
import scala.collection.mutable.HashMap
import scala.util.{Failure, Success, Try}

case class MiscReader(next: ReaderModule, root: ReaderModule) extends ReaderModule:

  def readerFn[T](ref: RTypeRef[T], isMapKey: Boolean = false)(using q: Quotes, tt: Type[T])(using cache: HashMap[Expr[TypedName], Expr[(JsonConfig, JsonParser) => Either[ParseError, ?]]]): Expr[(JsonConfig, JsonParser) => Either[ParseError, T]] =
    import quotes.reflect.*
    import Clazzes.*

    ref match
      case t: ScalaOptionRef[T] =>
        if isMapKey then throw new JsonError("Options cannot be map keys.")
        t.refType match
          case '[s] =>
            t.optionParamType.refType match
              case '[e] =>
                val subFn = readerFn[e](t.optionParamType.asInstanceOf[RTypeRef[e]]).asInstanceOf[Expr[(JsonConfig, JsonParser) => Either[ParseError, e]]]
                val isNullable = Expr(t.optionParamType.isNullable)
                '{ (j: JsonConfig, p: JsonParser) =>
                  p.nullCheck match {
                    case true if j.forbidNullsInInput => Left(JsonParseError(p.showError(s"Forbidden 'null' value received at position [${p.getPos}]")))
                    case true if j.noneAsNull         => Right(None.asInstanceOf[T])
                    case true if ! $isNullable        => Left(JsonParseError(p.showError(s"Null value given for non-nullable value type at position [${p.getPos}]")))
                    case true                         => Right(Some(null).asInstanceOf[T])
                    case false                        => $subFn(j, p).map(v => Some(v).asInstanceOf[T])
                  }
                }

      case t: AliasRef[T] =>
        t.refType match
          case '[s] =>
            t.unwrappedType.refType match
              case '[e] =>
                val subFn = readerFn[e](t.unwrappedType.asInstanceOf[RTypeRef[e]], isMapKey).asInstanceOf[Expr[(JsonConfig, JsonParser) => Either[ParseError, e]]]
                '{ (j: JsonConfig, p: JsonParser) => $subFn(j, p).asInstanceOf[Either[co.blocke.scalajack.json.ParseError, T]] }

      case t: SelfRefRef[T] =>
        if isMapKey then throw new JsonError("Class or trait types cannot be map keys.")
        t.refType match
          case '[s] =>
            val className = Expr(t.typedName.toString)
            '{ (j: JsonConfig, p: JsonParser) =>
              val cname = $className
              p.cache.get(cname.asInstanceOf[TypedName]) match
                case Some(fn) => fn(j, p).asInstanceOf[Either[co.blocke.scalajack.json.ParseError, T]]
                case None     => Left(JsonParseError(p.showError(s"Expected self-ref class $cname but none found in cache at position [${p.getPos}]")))
            }

      case t: TryRef[T] =>
        if isMapKey then throw new JsonError("Try values cannot be map keys.")
        t.refType match
          case '[s] =>
            t.tryRef.refType match
              case '[e] =>
                val subFn = readerFn[e](t.tryRef.asInstanceOf[RTypeRef[e]]).asInstanceOf[Expr[(JsonConfig, JsonParser) => Either[ParseError, e]]]
                val isNullable = Expr(t.tryRef.isNullable)
                '{ (j: JsonConfig, p: JsonParser) =>
                  p.nullCheck match {
                    case true if j.forbidNullsInInput                      => Left(JsonParseError(p.showError(s"Forbidden 'null' value received at position [${p.getPos}]")))
                    case true if j.tryFailureHandling == TryOption.AS_NULL => Right(Failure(JsonParseError(p.showError("null value received indicating Try failure"))).asInstanceOf[T])
                    case true if ! $isNullable                             => Left(JsonParseError(p.showError(s"Null value given for non-nullable value type at position [${p.getPos}]")))
                    case true                                              => Right(Success(null).asInstanceOf[T])
                    case false                                             => $subFn(j, p).map(v => Success(v).asInstanceOf[T])
                  }
                }

      case t =>
        next.readerFn[T](t)
