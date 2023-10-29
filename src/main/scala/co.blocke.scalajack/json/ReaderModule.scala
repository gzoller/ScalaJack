package co.blocke.scalajack
package json

import co.blocke.scala_reflection.{RTypeRef, TypedName}
import scala.quoted.*
import scala.collection.mutable.HashMap
import scala.util.{Failure, Success, Try}

trait ReaderModule:
  val root: ReaderModule
  def readerFn[T](ref: RTypeRef[T], isMapKey: Boolean = false)(using q: Quotes, tt: Type[T])(using cache: HashMap[Expr[TypedName], Expr[(JsonConfig, JsonParser) => Either[ParseError, ?]]]): Expr[(JsonConfig, JsonParser) => Either[ParseError, T]]

case class TerminusReaderModule(extension: Option[ReaderModule], root: ReaderModule) extends ReaderModule:
  def readerFn[T](ref: RTypeRef[T], isMapKey: Boolean = false)(using q: Quotes, tt: Type[T])(using cache: HashMap[Expr[TypedName], Expr[(JsonConfig, JsonParser) => Either[ParseError, ?]]]): Expr[(JsonConfig, JsonParser) => Either[ParseError, T]] =
    ref match
      case t =>
        val extResult = extension match
          case None      => Failure(JsonParseError("???")) // Should Never Happen(tm)
          case Some(ext) => Try(ext.readerFn[T](t))
        extResult match
          case Success(v) => v
          case Failure(_) =>
            val className = Expr(t.name)
            '{ (j: JsonConfig, p: JsonParser) => Left(JsonParseError("Unknown (or unsupported) RTypeRef class " + $className)) }
