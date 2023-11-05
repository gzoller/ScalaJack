package co.blocke.scalajack

import co.blocke.scala_reflection.{RTypeRef, TypedName}
import co.blocke.scala_reflection.reflect.ReflectOnType
import co.blocke.scala_reflection.reflect.rtypeRefs.ClassRef
import scala.collection.mutable.{HashMap, Map}
import scala.quoted.*
import quoted.Quotes
import json.*

object ScalaJack:

  inline def write[T](a: T)(using cfg: JsonConfig = JsonConfig()): String = ${ writeImpl[T]('a, 'cfg) }

  def writeImpl[T](aE: Expr[T], cfg: Expr[JsonConfig])(using q: Quotes, tt: Type[T]): Expr[String] =
    import q.reflect.*
    val ref = ReflectOnType(q)(TypeRepr.of[T], true)(using Map.empty[TypedName, Boolean]).asInstanceOf[RTypeRef[T]]
    val sbE = '{ new StringBuilder() }
    val classesSeen = Map.empty[TypedName, RTypeRef[?]]
    '{ ${ JsonWriter.refWrite[T](cfg, ref, aE, sbE)(using classesSeen) }.toString }

  // ---------------------------------------------------------------------

  inline def read[T](js: String)(using cfg: JsonConfig = JsonConfig()): T = ${ readImpl[T]('js, 'cfg) }

  def readImpl[T: Type](js: Expr[String], cfg: Expr[JsonConfig])(using q: Quotes): Expr[T] =
    import quotes.reflect.*

    val classRef = ReflectOnType[T](quotes)(TypeRepr.of[T], true)(using scala.collection.mutable.Map.empty[TypedName, Boolean])

    // Used to trap SelfRef's from going into endless loops and causing Stack Overflow.
    val seenBeforeFnCache = HashMap.empty[Expr[TypedName], Expr[(JsonConfig, JsonParser) => Either[ParseError, ?]]]

    val fn = JsonReader().readerFn[T](classRef)(using quotes, Type.of[T])(using seenBeforeFnCache)
    val listifiedCache = Expr.ofList(seenBeforeFnCache.toList.map(t => Expr.ofTuple(t)))

    '{ // run-time
      val parser = JsonParser($js, $listifiedCache.toMap)
      $fn($cfg, parser) match
        case Right(v) => v
        case Left(t)  => throw t
    }
