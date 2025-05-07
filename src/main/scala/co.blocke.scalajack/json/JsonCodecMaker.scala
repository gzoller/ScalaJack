package co.blocke.scalajack
package json

import writing.*
import reading.*
import co.blocke.scala_reflection.{RTypeRef, TypedName}
import co.blocke.scala_reflection.reflect.rtypeRefs.*
import co.blocke.scala_reflection.reflect.ReflectOnType
import co.blocke.scala_reflection.rtypes.{EnumRType, JavaClassRType, NonConstructorFieldInfo}
import co.blocke.scala_reflection.given
import reading.JsonSource
import scala.jdk.CollectionConverters.*
import scala.quoted.*
import scala.reflect.ClassTag
import scala.annotation.{switch, tailrec}
import scala.collection.Factory
import scala.util.{Failure, Success, Try}
import dotty.tools.dotc.ast.Trees.EmptyTree
import org.apache.commons.text.StringEscapeUtils
import org.apache.commons.lang3.text.translate.CharSequenceTranslator
import dotty.tools.dotc.core.TypeComparer.AnyConstantType
import scala.jdk.CollectionConverters.*

object JsonCodecMaker:

  def generateCodecFor[T](ctx: CodecBuildContext, ref: RTypeRef[T], cfg: SJConfig)(using Type[T]): Expr[JsonCodec[T]] =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.*

    // ================================================================
    // We generate a codec class and then kick off a deep traversal of
    // generation from the given root ref (refer waaay back at the top of this fn...).
    // ================================================================
    val codecDef = '{ // FIXME: generate a type class instance using `ClassDef.apply` and `Symbol.newClass` calls after graduating from experimental API: https://www.scala-lang.org/blog/2022/06/21/scala-3.1.3-released.html
      new JsonCodec[T] {
        def encodeValue(in: T, out: JsonOutput): Unit = ${ Writer.genWriteVal(ctx, cfg, 'in, ref, 'out) }
        def decodeValue(in: JsonSource): T = ${ Reader.genReadVal(ctx, cfg, ref, 'in).asExprOf[T] }
      }
    }.asTerm

    val readerMapValDef = {
      val entries: List[Expr[(String, JsonSource => Any)]] =
        ctx.readerFnMap.collect { case (key, RealReader(fnExpr, _)) =>
          val widened = fnExpr.asExprOf[JsonSource => Any]
          '{ ${ Expr(key.toString) } -> $widened }
        }.toList

      val mapExpr: Expr[Map[String, JsonSource => Any]] =
        '{ Map.from[String, JsonSource => Any](${ Expr.ofList(entries) }) }

      ValDef(ctx.readerMapSym, Some(mapExpr.asTerm))
    }

    val writerMapExpr: Expr[Map[String, (Any, JsonOutput) => Unit]] =
      '{
        Map[String, (Any, JsonOutput) => Unit](
          ${
            Expr.ofList(
              ctx.writerFnMapEntries.toList.map { case (k, v) =>
                '{ ${ Expr(k.toString) } -> $v }
              }
            )
          }*
        )
      }
    val writerMapDef = ValDef(
      ctx.writerMapSym,
      Some(writerMapExpr.asTerm)
    )

    val mapDefs =
      if ctx.seenSelfRef then List(readerMapValDef) ++ List(writerMapDef)
      else Nil

    val codec = Block(
      // 🧨 This MUST be first — so any methods can reference it
      ctx.classFieldMatrixValDefs.values.toList ++
        mapDefs ++

        // Functions (can reference anything above)
        ctx.writeMethodDefs.values ++
        ctx.readMethodDefs.values.toList ++ {
          if ctx.seenAnyRef then List(ctx.readAnyDef)
          else Nil
        },
      codecDef
    ).asExprOf[JsonCodec[T]]

//    if ref.name.contains("AnyShell") then println(s"Codec: ${codec.show}")
    codec
