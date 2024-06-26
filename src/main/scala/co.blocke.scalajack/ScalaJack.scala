package co.blocke.scalajack

import co.blocke.scala_reflection.{RTypeRef, TypedName}
import co.blocke.scala_reflection.reflect.ReflectOnType
import scala.quoted.*

import quoted.Quotes
import json.*

case class ScalaJack[T](jsonCodec: JsonCodec[T]):
  def fromJson(js: String): T =
    jsonCodec.decodeValue(reading.JsonSource(js))

  val out = writing.JsonOutput()
  def toJson(a: T): String =
    jsonCodec.encodeValue(a, out.clear())
    out.result

// ---------------------------------------

object ScalaJack:

  // ----- Use default JsonConfig
  inline def sjCodecOf[T]: ScalaJack[T] = ${ codecOfImpl[T] }
  def codecOfImpl[T: Type](using Quotes): Expr[ScalaJack[T]] =
    import quotes.reflect.*
    val classRef = ReflectOnType[T](quotes)(TypeRepr.of[T], true)(using scala.collection.mutable.Map.empty[TypedName, Boolean])
    val jsonCodec = JsonCodecMaker.generateCodecFor(classRef, SJConfig)

    '{ ScalaJack($jsonCodec) }

  // ----- Use given JsonConfig
  inline def sjCodecOf[T](inline cfg: SJConfig): ScalaJack[T] = ${ codecOfImplWithConfig[T]('cfg) }
  def codecOfImplWithConfig[T: Type](cfgE: Expr[SJConfig])(using Quotes): Expr[ScalaJack[T]] =
    import quotes.reflect.*
    val cfg = summon[FromExpr[SJConfig]].unapply(cfgE)
    val classRef = ReflectOnType[T](quotes)(TypeRepr.of[T], true)(using scala.collection.mutable.Map.empty[TypedName, Boolean])
    val jsonCodec = JsonCodecMaker.generateCodecFor(classRef, cfg.getOrElse(SJConfig))
    '{ ScalaJack($jsonCodec) }
