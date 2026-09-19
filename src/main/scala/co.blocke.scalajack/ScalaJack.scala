package co.blocke.scalajack

import co.blocke.scala_reflection.reflect.ReflectOnType
import co.blocke.scalajack.json.writing.JsonOutput

import scala.quoted.*
import quoted.Quotes
import json.*

case class ScalaJack[T](jsonCodec: JsonCodec[T], listCodec: JsonCodec[List[T]]):

  def toJson(a: T, out: JsonOutput = null): String =
    ScalaJack.withJsonOutput(out) { target =>
      jsonCodec.encodeValue(a, target.clear())
      target.result
    }

  def fromJson(js: String): T =
    jsonCodec.decodeValue(json.reading.JsonSource(js))

  def toJsonList(xs: List[T], out: JsonOutput = null): String =
    ScalaJack.withJsonOutput(out) { target =>
      listCodec.encodeValue(xs, target.clear())
      target.result
    }

  def fromJsonList(js: String): List[T] =
    listCodec.decodeValue(json.reading.JsonSource(js))

// ---------------------------------------

object ScalaJack {

  final private class JsonOutputPool:
    private val primary = JsonOutput()
    private var primaryInUse = false
    private var nested: java.util.ArrayDeque[JsonOutput] = null

    def acquire(): JsonOutput =
      if !primaryInUse then
        primaryInUse = true
        primary
      else
        if nested == null then nested = new java.util.ArrayDeque[JsonOutput]()
        val output = nested.pollFirst()
        if output == null then JsonOutput() else output

    def release(output: JsonOutput): Unit =
      if output eq primary then primaryInUse = false
      else nested.addFirst(output)

  private val jsonOutputPool = ThreadLocal.withInitial(() => new JsonOutputPool())

  private[scalajack] def withJsonOutput[A](provided: JsonOutput)(f: JsonOutput => A): A =
    if provided != null then f(provided)
    else
      val pool = jsonOutputPool.get()
      val target = pool.acquire()
      try f(target)
      finally pool.release(target)

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
}
