package co.blocke.scalajack

import co.blocke.scala_reflection.{RTypeRef, TypedName}
import co.blocke.scala_reflection.reflect.ReflectOnType
import scala.quoted.*

import quoted.Quotes
import json.*

case class ScalaJack[T](jsonDecoder: reading.JsonDecoder[T], jsonEncoder: JsonCodec[T]): // extends JsonCodec[T] //with YamlCodec with MsgPackCodec
  def fromJson(js: String)(using cfg: JsonConfig = JsonConfig()): Either[JsonParseError, T] =
    jsonDecoder.decodeJson(js)

  val out = writing.JsonOutput() // let's clear & re-use JsonOutput--avoid re-allocating all the internal buffer space
  def toJson(a: T)(using cfg: JsonConfig = JsonConfig()): String =
    jsonEncoder.encodeValue(a, out.clear())
    out.result

// ---------------------------------------

object ScalaJack:

  def apply[A](implicit a: ScalaJack[A]): ScalaJack[A] = a

  inline def sj[T]: ScalaJack[T] = ${ sjImpl[T] }

  def sjImpl[T](using q: Quotes, tt: Type[T]): Expr[ScalaJack[T]] =
    import q.reflect.*
    val classRef = ReflectOnType[T](quotes)(TypeRepr.of[T], true)(using scala.collection.mutable.Map.empty[TypedName, Boolean])
    val jsonDecoder = reading.JsonReader.refRead(classRef)
    val jsonEncoder = writing.JsonCodecMaker.generateCodecFor(classRef)

    '{ ScalaJack($jsonDecoder, $jsonEncoder) }

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
