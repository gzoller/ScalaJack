package co.blocke.scalajack

import co.blocke.scala_reflection.{RTypeRef, TypedName}
import co.blocke.scala_reflection.reflect.ReflectOnType
import co.blocke.scalajack.json.writing.JsonOutput

import scala.quoted.*
import quoted.Quotes
import json.*

import scala.collection.mutable

case class ScalaJack[T](jsonCodec: JsonCodec[T]):
  def fromJson(js: String): T =
    jsonCodec.decodeValue(reading.JsonSource(js))

  val out: JsonOutput = writing.JsonOutput()

  def toJson(a: T): String =
    jsonCodec.encodeValue(a, out.clear())
    out.result

// ---------------------------------------

object ScalaJack:

  // ----- Use default JsonConfig
  inline def sjCodecOf[T]: ScalaJack[T] = ${ codecOfImpl[T] }
  private def codecOfImpl[T: Type](using q: Quotes): Expr[ScalaJack[T]] =
    val ctx = new CodecBuildContext
    import ctx.quotes.reflect.*
    val classRef = ReflectOnType[T](ctx.quotes)(TypeRepr.of[T], true)(using ctx.seenBefore)
    val jsonCodec = JsonCodecMaker.generateCodecFor(ctx, classRef, SJConfig)

    '{ ScalaJack($jsonCodec) }

  // ----- Use given JsonConfig
  inline def sjCodecOf[T](inline cfg: SJConfig): ScalaJack[T] = ${ codecOfImplWithConfig[T]('cfg) }
  private def codecOfImplWithConfig[T: Type](cfgE: Expr[SJConfig])(using q: Quotes): Expr[ScalaJack[T]] =
    val ctx = new CodecBuildContext
    import ctx.quotes.reflect.*
    val cfg = summon[FromExpr[SJConfig]].unapply(cfgE)
    val classRef = ReflectOnType[T](ctx.quotes)(TypeRepr.of[T], true)(using ctx.seenBefore)
    val jsonCodec = JsonCodecMaker.generateCodecFor(ctx, classRef, cfg.getOrElse(SJConfig))
    '{ ScalaJack($jsonCodec) }

  inline def makeRecursiveSum(): Int => Int = ${ makeRecursiveSumImpl }

  def makeRecursiveSumImpl(using Quotes): Expr[Int => Int] =
    import quotes.reflect.*

    // Define symbol for the recursive function: def sum(n: Int): Int = ...
    val sumSym = Symbol.newMethod(
      Symbol.spliceOwner,
      "sum",
      MethodType(List("n"))(
        _ => List(TypeRepr.of[Int]),
        _ => TypeRepr.of[Int]
      )
    )

    val nSym = sumSym.paramSymss.head.head
    val nRef = Ref(nSym)

    val cond = Apply(
      Select.overloaded(nRef, "<=", Nil, List(TypeTree.of[Int].asTerm)),
      List(Literal(IntConstant(0)))
    )

    val subOne = Apply(
      Select.overloaded(nRef, "-", Nil, List(TypeTree.of[Int].asTerm)),
      List(Literal(IntConstant(1)))
    )

    val recursiveCall = Apply(Ref(sumSym), List(subOne))

    val result = Apply(
      Select.overloaded(nRef, "+", Nil, List(TypeTree.of[Int].asTerm)),
      List(recursiveCall)
    )

    val body = If(cond, Literal(IntConstant(0)), result)

    val defDef = DefDef(sumSym, { case List(List(nVal)) => Some(body) })

    // Create a lambda: (n: Int) => sum(n)
    val lambdaSym = Symbol.newMethod(
      Symbol.spliceOwner,
      "lambda",
      MethodType(List("n"))(
        _ => List(TypeRepr.of[Int]),
        _ => TypeRepr.of[Int]
      )
    )
    val paramSym = lambdaSym.paramSymss.head.head
    val paramRef = Ref(paramSym)
    val lambdaBody = Apply(Ref(sumSym), List(paramRef))

    val lambdaDef = DefDef(lambdaSym, { case List(List(vd)) => Some(lambdaBody) })

    val block = Block(List(defDef, lambdaDef), Closure(Ref(lambdaSym), None))

    println(s"[🧩] Final generated code:\n${block.show(using Printer.TreeStructure)}")

    block.asExprOf[Int => Int]
