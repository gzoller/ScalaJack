package co.blocke.scalajack

import co.blocke.scala_reflection.TypedName
import co.blocke.scala_reflection.reflect.ReflectOnType
import co.blocke.scala_reflection.reflect.rtypeRefs.ClassRef
import scala.collection.mutable.HashMap
import scala.quoted.*
import quoted.Quotes
import json.*

object ScalaJack:

  inline def write[T](t: T)(using cfg: JsonConfig = JsonConfig()): String = ${ writeImpl[T]('t, 'cfg) }

  def writeImpl[T: Type](t: Expr[T], cfg: Expr[JsonConfig])(using q: Quotes): Expr[String] =
    import quotes.reflect.*

    val rtRef = ReflectOnType[T](q)(TypeRepr.of[T])(using scala.collection.mutable.Map.empty[TypedName, Boolean])
    val fn = JsonWriter.writeJsonFn[T](rtRef)
    '{
      val sb = new StringBuilder()
      $fn($t, sb, $cfg).toString
    }

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

    /*
  inline def foo[T](str: String)(using cfg: Config): T = ${ fooImpl[T]('str, 'cfg) }

  def fooImpl[T: Type](str: Expr[String], cfg: Expr[Config])(using q: Quotes): Expr[T] =
    import quotes.reflect.*

    // How can I get some configuration here???

    '{ // run-time
      doSomething($str, $cfg) // can use str and cfg here
    }

    Parameters may only be:
     * Quoted parameters or fields
     * Literal values of primitive types
     * References to `inline val`s
     */
