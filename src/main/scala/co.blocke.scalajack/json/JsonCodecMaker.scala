package co.blocke.scalajack
package json

import writing.*
import reading.*
import shared.CodecBuildContext
import co.blocke.scala_reflection.RTypeRef
import reading.JsonSource
import scala.quoted.*

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

    val codec = Block(
      // Functions (can reference anything above)
      ctx.writeMethodDefs.values.toList ++
        ctx.readMethodDefs.values.toList ++ {
          if ctx.seenAnyRef then List(ctx.readAnyDef)
          else Nil
        },
      codecDef
    ).asExprOf[JsonCodec[T]]

//    if ref.name.contains("Outer") then println(s"Codec: ${codec.show}")
    codec
