package co.blocke.scalajack
package json

import co.blocke.scala_reflection.{TypedName, RTypeRef}
import scala.quoted.*
import scala.collection.mutable.HashMap
import scala.util.{Try, Success, Failure}

trait ReaderModule:
  val root: ReaderModule
  def readerFn[T](ref: RTypeRef[T], isMapKey: Boolean = false)(using q: Quotes, tt: Type[T])(using cache: HashMap[Expr[TypedName], Expr[(JsonConfig, JsonParser) => Either[ParseError, ?]]]): Expr[(JsonConfig, JsonParser) => Either[ParseError, T]]

case class TerminusReaderModule(extension: Option[ReaderModule], root: ReaderModule) extends ReaderModule:
  def readerFn[T](ref: RTypeRef[T], isMapKey: Boolean = false)(using q: Quotes, tt: Type[T])(using cache: HashMap[Expr[TypedName], Expr[(JsonConfig, JsonParser) => Either[ParseError, ?]]]): Expr[(JsonConfig, JsonParser) => Either[ParseError, T]] =
    ref match
      case t =>
        val extResult = extension match
            case None => Failure(ParseError("???"))
            case Some(ext) => Try(ext.readerFn[T](t))
        extResult match
            case Success(v) => v
            case Failure(_) =>
                val className = Expr(t.name) 
                '{ (j: JsonConfig, p: JsonParser) => Left(ParseError("Unknown (or unsupported) RTypeRef class " + $className)) }
