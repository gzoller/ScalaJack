package co.blocke.scalajack

import co.blocke.scala_reflection.{RTypeRef, TypedName}
import co.blocke.scala_reflection.reflect.ReflectOnType
import scala.quoted.*

import quoted.Quotes
import json.*

case class ScalaJack[T](jsonDecoder: reading.JsonDecoder[T], jsonEncoder: (T, writing.JsonOutput, JsonConfig) => String): // extends JsonCodec[T] //with YamlCodec with MsgPackCodec
  def fromJson(js: String)(using cfg: JsonConfig = JsonConfig()): Either[JsonParseError, T] =
    jsonDecoder.decodeJson(js)

  def toJson(a: T)(using cfg: JsonConfig = JsonConfig()): String =
    jsonEncoder(a, writing.JsonOutput(), cfg)

// ---------------------------------------

object ScalaJack:

  def apply[A](implicit a: ScalaJack[A]): ScalaJack[A] = a

  inline def sj[T]: ScalaJack[T] = ${ sjImpl[T] }

  def sjImpl[T](using q: Quotes, tt: Type[T]): Expr[ScalaJack[T]] =
    import q.reflect.*
    val classRef = ReflectOnType[T](quotes)(TypeRepr.of[T], true)(using scala.collection.mutable.Map.empty[TypedName, Boolean])
    val jsonDecoder = reading.JsonReader.refRead(classRef)
    val jsonEncoder = writing.JsonWriter.refRead(classRef)

    '{ ScalaJack($jsonDecoder, $jsonEncoder) }

  //   refRead[T](classRef)

  // private def refRead[T](ref: RTypeRef[T])(using Quotes): Expr[ScalaJack[T]] = ???

/*
  // ---------------------------------------------------------------------


  inline def inspect[T]: sj[T] = ${ inspectImpl[T] }

  def inspectImpl[T](using q: Quotes, tt: Type[T]): Expr[sj[T]] =
    import q.reflect.*
    val classRef = ReflectOnType[T](quotes)(TypeRepr.of[T], true)(using scala.collection.mutable.Map.empty[TypedName, Boolean])
    JsonReader.refRead[T](classRef)

  inline def write[T](a: T)(using cfg: JsonConfig = JsonConfig()): String = ${ writeImpl[T]('a, 'cfg) }

  def writeImpl[T](aE: Expr[T], cfg: Expr[JsonConfig])(using q: Quotes, tt: Type[T]): Expr[String] =
    import q.reflect.*
    val ref = ReflectOnType(q)(TypeRepr.of[T], true)(using Map.empty[TypedName, Boolean]).asInstanceOf[RTypeRef[T]]
    val classesSeen = Map.empty[TypedName, RTypeRef[?]]

    val sbE = '{ new StringBuilder() }
    '{ ${ JsonWriter.refWrite[T](cfg, ref, aE, sbE)(using classesSeen) }.result }

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

/*

    implicit val codec: sj[Record] = ScalaJack.inspect[Record]

    (optional given cfg)

    sj[Record].read(js)
    sj[Record].readYml(yml)
    sj[Record].readMP(msgPack)

    sj[Record].write(thing)
    sj[Record].writeYml(thing)
    sj[Record].writeMP(thing)


    trait sj[A] extends JSModule with YMLModule with MPModule:
      ...
 */
