package co.blocke.scalajack

import co.blocke.scala_reflection.{RTypeRef, TypedName}
import co.blocke.scala_reflection.reflect.ReflectOnType
import scala.quoted.*

import quoted.Quotes
import json.*

case class ScalaJack[T](jsonCodec: JsonCodec[T]): // extends JsonCodec[T] //with YamlCodec with MsgPackCodec
  def fromJson(js: String): T = // Either[JsonParseError, T] =
    jsonCodec.decodeValue(reading.JsonSource(js, js.getBytes))

  val out = writing.JsonOutput() // let's clear & re-use JsonOutput--avoid re-allocating all the internal buffer space
  def toJson(a: T): String =
    jsonCodec.encodeValue(a, out.clear())
    out.result
/*
case class ScalaJack[T](jsonDecoder: reading.JsonDecoder[T], jsonEncoder: JsonCodec[T]): // extends JsonCodec[T] //with YamlCodec with MsgPackCodec
  def fromJson(js: String): Either[JsonParseError, T] =
    jsonDecoder.decodeJson(js)

  val out = writing.JsonOutput() // let's clear & re-use JsonOutput--avoid re-allocating all the internal buffer space
  def toJson(a: T): String =
    jsonEncoder.encodeValue(a, out.clear())
    out.result
 */

// ---------------------------------------

object ScalaJack:

  def apply[A](implicit a: ScalaJack[A]): ScalaJack[A] = a

  // ----- Use default JsonConfig
  inline def sj[T]: ScalaJack[T] = ${ sjImpl[T] }
  def sjImpl[T: Type](using Quotes): Expr[ScalaJack[T]] =
    import quotes.reflect.*
    val classRef = ReflectOnType[T](quotes)(TypeRepr.of[T], true)(using scala.collection.mutable.Map.empty[TypedName, Boolean])
    // val jsonDecoder = reading.JsonReader.refRead2(classRef)
    // println(s"Decoder: ${jsonDecoder.show}")
    val jsonCodec = JsonCodecMaker.generateCodecFor(classRef, JsonConfig)

    '{ ScalaJack($jsonCodec) }

  // ----- Use given JsonConfig
  inline def sj[T](inline cfg: JsonConfig): ScalaJack[T] = ${ sjImplWithConfig[T]('cfg) }
  def sjImplWithConfig[T: Type](cfgE: Expr[JsonConfig])(using Quotes): Expr[ScalaJack[T]] =
    import quotes.reflect.*
    val cfg = summon[FromExpr[JsonConfig]].unapply(cfgE)
    val classRef = ReflectOnType[T](quotes)(TypeRepr.of[T], true)(using scala.collection.mutable.Map.empty[TypedName, Boolean])
    // val jsonDecoder = reading.JsonReader.refRead2(classRef)
    val jsonCodec = JsonCodecMaker.generateCodecFor(classRef, cfg.getOrElse(JsonConfig))
    '{ ScalaJack($jsonCodec) }

  //   refRead[T](classRef)

  // private def refRead[T](ref: RTypeRef[T])(using Quotes): Expr[ScalaJack[T]] = ???

/*
  // ---------------------------------------------------------------------


  inline def read[T](js: String)(using cfg: JsonConfig = JsonConfig()): Either[ParseError, T] = ${ readImpl[T]('js, 'cfg) }

  def readImpl[T: Type](js: Expr[String], cfg: Expr[JsonConfig])(using q: Quotes): Expr[Either[ParseError, T]] =
    import quotes.reflect.*
    val classRef = ReflectOnType[T](quotes)(TypeRepr.of[T], true)(using scala.collection.mutable.Map.empty[TypedName, Boolean])
    val decoder = JsonReader.refRead[T](classRef)
    '{
      $decoder.decodeJson($js)
    }
 */
