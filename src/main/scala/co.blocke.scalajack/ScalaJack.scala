package co.blocke.scalajack

import co.blocke.scala_reflection.{RTypeRef, TypedName}
import co.blocke.scala_reflection.reflect.ReflectOnType
import scala.quoted.*

import quoted.Quotes
import json.*
import msgpack.*

case class ScalaJack[T](jsonCodec: JsonCodec[T], msgPackCodec: MsgPackCodec[T]): // extends JsonCodec[T] //with YamlCodec with MsgPackCodec
  def fromJson(js: String): T =
    jsonCodec.decodeValue(reading.JsonSource(js))

  val out = writing.JsonOutput()
  def toJson(a: T): String =
    jsonCodec.encodeValue(a, out.clear())
    out.result

  val outArray = new org.msgpack.core.buffer.ArrayBufferOutput()
  def toMsgPack(a: T): Array[Byte] =
    outArray.clear()
    val outMP = org.msgpack.core.MessagePack.newDefaultPacker(outArray)
    msgPackCodec.encodeValue(a, outMP)
    outMP.close()
    outArray.toByteArray

  def fromMsgPack(a: Array[Byte]): T =
    val inMP = org.msgpack.core.MessagePack.newDefaultUnpacker(a)
    msgPackCodec.decodeValue(inMP)

// ---------------------------------------

object ScalaJack:

  def apply[A](implicit a: ScalaJack[A]): ScalaJack[A] = a

  // ----- Use default JsonConfig
  inline def sjCodecOf[T]: ScalaJack[T] = ${ codecOfImpl[T] }
  def codecOfImpl[T: Type](using Quotes): Expr[ScalaJack[T]] =
    import quotes.reflect.*
    val classRef = ReflectOnType[T](quotes)(TypeRepr.of[T], true)(using scala.collection.mutable.Map.empty[TypedName, Boolean])
    val jsonCodec = JsonCodecMaker.generateCodecFor(classRef, SJConfig)
    val mpCodec = MsgPackCodecMaker.generateCodecFor(classRef, SJConfig)

    '{ ScalaJack($jsonCodec, $mpCodec) }

  // ----- Use given JsonConfig
  inline def sjCodecOf[T](inline cfg: SJConfig): ScalaJack[T] = ${ codecOfImplWithConfig[T]('cfg) }
  def codecOfImplWithConfig[T: Type](cfgE: Expr[SJConfig])(using Quotes): Expr[ScalaJack[T]] =
    import quotes.reflect.*
    val cfg = summon[FromExpr[SJConfig]].unapply(cfgE)
    val classRef = ReflectOnType[T](quotes)(TypeRepr.of[T], true)(using scala.collection.mutable.Map.empty[TypedName, Boolean])
    val jsonCodec = JsonCodecMaker.generateCodecFor(classRef, cfg.getOrElse(SJConfig))
    val mpCodec = MsgPackCodecMaker.generateCodecFor(classRef, SJConfig)
    '{ ScalaJack($jsonCodec, $mpCodec) }
