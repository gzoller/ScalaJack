package co.blocke.scalajack

import co.blocke.scala_reflection.reflect.ReflectOnType
import co.blocke.scalajack.json.writing.JsonOutput

import scala.quoted.*
import quoted.Quotes
import json.*
import xml.*

case class ScalaJack[T](jsonCodec: JsonCodec[T], listCodec: JsonCodec[List[T]]):

  val out: JsonOutput = json.writing.JsonOutput()

  def toJson(a: T): String =
    jsonCodec.encodeValue(a, out.clear())
    out.result

  def fromJson(js: String): T =
    jsonCodec.decodeValue(json.reading.JsonSource(js))

  def toJsonList(xs: List[T]): String =
    listCodec.encodeValue(xs, out.clear())
    out.result

  def fromJsonList(js: String): List[T] =
    listCodec.decodeValue(json.reading.JsonSource(js))

case class ScalaJackXML[T](xmlCodec: XmlCodec[T]):
  def fromXml(x: String): T =
    xmlCodec.decodeValue(xml.reading.XmlSource(x))

  val out: xml.writing.XmlOutput = xml.writing.XmlOutput()

  def toXml(a: T): String =
    xmlCodec.encodeValue(a, out.clear())
    out.result

// ---------------------------------------

object ScalaJack {

  // -----------------------
  //         JSON
  // -----------------------

  // ----- Use default JsonConfig
  inline def sjCodecOf[T]: ScalaJack[T] = ${ codecOfImpl[T] }

  private def codecOfImpl[T: Type](using q: Quotes): Expr[ScalaJack[T]] =
    val ctx = new JsonCodecBuildContext()
    import ctx.quotes.reflect.*
    val classRef = ReflectOnType[T](ctx.quotes)(TypeRepr.of[T], true)(using ctx.seenBefore)
    val jsonCodec = JsonCodecMaker.generateCodecFor(ctx, classRef, SJConfig)
    val listCodecExpr = makeListCodec(jsonCodec)
    '{ ScalaJack($jsonCodec, $listCodecExpr) }

  // ----- Use given JsonConfig
  inline def sjCodecOf[T](inline cfg: SJConfig): ScalaJack[T] = ${ codecOfImplWithConfig[T]('cfg) }

  private def codecOfImplWithConfig[T: Type](cfgE: Expr[SJConfig])(using q: Quotes): Expr[ScalaJack[T]] =
    val ctx = new JsonCodecBuildContext()
    import ctx.quotes.reflect.*
    val cfg = summon[FromExpr[SJConfig]].unapply(cfgE)
    val classRef = ReflectOnType[T](ctx.quotes)(TypeRepr.of[T], true)(using ctx.seenBefore)
    val jsonCodec = JsonCodecMaker.generateCodecFor(ctx, classRef, cfg.getOrElse(SJConfig))
    val listCodecExpr = makeListCodec(jsonCodec)
    '{ ScalaJack($jsonCodec, $listCodecExpr) }

  private def makeListCodec[T: Type](elemCodec: Expr[JsonCodec[T]])(using Quotes): Expr[JsonCodec[List[T]]] =
    '{
      new JsonCodec[List[T]]:
        def encodeValue(xs: List[T], out: JsonOutput) =
          if xs == null then out.burpNull()
          else
            out.startArray()
            xs.foreach(x => $elemCodec.encodeValue(x, out))
            out.endArray()

        def decodeValue(in: json.reading.JsonSource): List[T] =
          val buf = in.expectArray(() => $elemCodec.decodeValue(in))
          if buf == null then null else buf.toList
    }

  // -----------------------
  //         XML
  // -----------------------

  // ----- Use default XmlConfig
  inline def sjXmlCodecOf[T]: ScalaJackXML[T] = ${ xmlCodecOfImpl[T] }

  private def xmlCodecOfImpl[T: Type](using q: Quotes): Expr[ScalaJackXML[T]] =
    val ctx = new XmlCodecBuildContext()
    import ctx.quotes.reflect.*
    val classRef = ReflectOnType[T](ctx.quotes)(TypeRepr.of[T], true)(using ctx.seenBefore)
    val xmlCodec = XmlCodecMaker.generateCodecFor(ctx, classRef, SJConfig)
    '{ ScalaJackXML($xmlCodec) }

  // ----- Use given JsonConfig
  inline def sjXmlCodecOf[T](inline cfg: SJConfig): ScalaJackXML[T] = ${ xmlCodecOfImplWithConfig[T]('cfg) }

  private def xmlCodecOfImplWithConfig[T: Type](cfgE: Expr[SJConfig])(using q: Quotes): Expr[ScalaJackXML[T]] =
    val ctx = new XmlCodecBuildContext()
    import ctx.quotes.reflect.*
    val cfg = summon[FromExpr[SJConfig]].unapply(cfgE)
    val classRef = ReflectOnType[T](ctx.quotes)(TypeRepr.of[T], true)(using ctx.seenBefore)
    val xmlCodec = XmlCodecMaker.generateCodecFor(ctx, classRef, cfg.getOrElse(SJConfig))
    '{ ScalaJackXML($xmlCodec) }

}
