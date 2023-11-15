package co.blocke.scalajack

import co.blocke.scala_reflection.{RTypeRef, TypedName}
import co.blocke.scala_reflection.reflect.ReflectOnType
import co.blocke.scala_reflection.reflect.rtypeRefs.ClassRef
import scala.collection.mutable.{HashMap, Map}
import scala.quoted.*
import quoted.Quotes
import json.*

object sj: // Shorter and "lighter" than "ScalaJack" everywhere.

  inline def write[T](a: T)(using cfg: JsonConfig = JsonConfig()): String = ${ writeImpl[T]('a, 'cfg) }

  def writeImpl[T](aE: Expr[T], cfg: Expr[JsonConfig])(using q: Quotes, tt: Type[T]): Expr[String] =
    import q.reflect.*
    val ref = ReflectOnType(q)(TypeRepr.of[T], true)(using Map.empty[TypedName, Boolean]).asInstanceOf[RTypeRef[T]]
    val sbE = '{ new StringBuilder() }
    val classesSeen = Map.empty[TypedName, RTypeRef[?]]
    '{ ${ JsonWriter.refWrite[T](cfg, ref, aE, sbE)(using classesSeen) }.toString }

  // ---------------------------------------------------------------------

  inline def read[T](js: String)(using cfg: JsonConfig = JsonConfig()): Either[ParseError, T] = ${ readImpl[T]('js, 'cfg) }

  def readImpl[T: Type](js: Expr[String], cfg: Expr[JsonConfig])(using q: Quotes): Expr[Either[ParseError, T]] =
    import quotes.reflect.*

    try {
      val classRef = ReflectOnType[T](quotes)(TypeRepr.of[T], true)(using scala.collection.mutable.Map.empty[TypedName, Boolean])
      val decoder = JsonReader.refRead[T](classRef)
      '{
        $decoder.decodeJson($js)
      }
    } catch {
      case t: Throwable =>
        val error = Expr(t.getClass.getName())
        val msg = Expr(t.getMessage())
        '{ Left(ParseError($error + " was thrown with message " + $msg)) }
    }
