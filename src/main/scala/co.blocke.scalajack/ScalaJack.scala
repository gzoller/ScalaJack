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

  inline def read[T](js: String)(using cfg: JsonConfig = JsonConfig()): T = ${ readImpl[T]('js, 'cfg) }

  def readImpl[T: Type](js: Expr[String], cfg: Expr[JsonConfig])(using q: Quotes): Expr[T] =
    import quotes.reflect.*

    val classRef = ReflectOnType[T](quotes)(TypeRepr.of[T], true)(using scala.collection.mutable.Map.empty[TypedName, Boolean])

    // Used to trap SelfRef's from going into endless loops and causing Stack Overflow.
    val seenBeforeFnCache = HashMap.empty[Expr[TypedName], Expr[(JsonConfig, JsonParser) => Either[ParseError, ?]]]

    //  def refFn[T: Type](ref: RTypeRef[T])(using q: Quotes)(using cache: HashMap[Expr[TypedName], Expr[(JsonConfig, JsonParser) => Either[ParseError, ?]]]): Expr[(JsonConfig, JsonParser) => Either[ParseError, T]] =
    val fn = JsonReader.refFn[T](classRef)(using quotes, Type.of[T])(using seenBeforeFnCache)
    val listifiedCache = Expr.ofList(seenBeforeFnCache.toList.map(t => Expr.ofTuple(t)))

    '{ // run-time
      val parser = JsonParser($js, $listifiedCache.toMap)
      $fn($cfg, parser) match
        case Right(v) => v
        case Left(t)  => throw t

      // val root = RootEnvelope($js, $fn)
      // root.run[T]($cfg, root.parser) match
      //   case Right(v) => v
      //   case Left(t)  => throw t
    }

// abstract class Envelope:
//   val rootEnvelope: Option[RootEnvelope]
//   val innerFn: (JsonConfig, JsonParser) => Either[ParseError, ?]
//   def run[T](j: JsonConfig, p: JsonParser): Either[ParseError, T]

// case class RootEnvelope(js: String, fn: (JsonConfig, JsonParser) => Either[ParseError, ?]) extends Envelope:
//   val rootEnvelope: Option[RootEnvelope] = None
//   val seenBefore: scala.collection.mutable.HashMap[TypedName, (JsonConfig, JsonParser) => Either[ParseError, ?]] =
//     scala.collection.mutable.HashMap.empty[TypedName, (JsonConfig, JsonParser) => Either[ParseError, ?]]
//   val parser = JsonParser(js)
//   val innerFn: (JsonConfig, JsonParser) => Either[ParseError, ?] = fn
//   def run[T](j: JsonConfig, p: JsonParser): Either[ParseError, T] = innerFn(j, p).map(_.asInstanceOf[T])

// case class SimpleEnvelope(fn: (JsonConfig, JsonParser) => Either[ParseError, ?], root: RootEnvelope) extends Envelope:
//   val rootEnvelope = Some(root)
//   val innerFn = fn
//   def run[T](j: JsonConfig, p: JsonParser): Either[ParseError, T] = innerFn(j, p).map(_.asInstanceOf[T])
