package co.blocke.scalajack

import co.blocke.scala_reflection.TypedName
import co.blocke.scala_reflection.reflect.ReflectOnType
import co.blocke.scala_reflection.reflect.rtypeRefs.ClassRef
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

  inline def read[T](js: String)(using cfg: JsonConfig = JsonConfig()): T = ${ readImpl[T]('js, 'cfg) }

  def readImpl[T: Type](js: Expr[String], cfg: Expr[JsonConfig])(using q: Quotes): Expr[T] =
    import quotes.reflect.*

    val classRef = ReflectOnType[T](quotes)(TypeRepr.of[T], true)(using scala.collection.mutable.Map.empty[TypedName, Boolean]).asInstanceOf[ClassRef[T]]
    val parseTable = JsonReader.classParseMap[T](classRef)
    val instantiator = JsonReader.classInstantiator[T](classRef)
    '{ // run-time
      val parser = JsonParser($js)
      val classFieldMap = $parseTable(parser) // Map[String, JsonConfig => Either[ParseError, ?]]
      parser.expectClass[T]($cfg, classFieldMap, $instantiator) match
        case Right(v) => v
        case Left(t) =>
          println("BOOM: " + t.msg)
          throw t
    }
