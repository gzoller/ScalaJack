package co.blocke.scalajack
package xml

import writing.*
import reading.*
import co.blocke.scala_reflection.RTypeRef
import scala.quoted.*
import shared.CodecBuildContext

object XmlCodecMaker:

  def generateCodecFor[T](ctx: CodecBuildContext, ref: RTypeRef[T], cfg: SJConfig)(using Type[T]): Expr[XmlCodec[T]] =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.*

    // ================================================================
    // We generate a codec class and then kick off a deep traversal of
    // generation from the given root ref (refer waaay back at the top of this fn...).
    // ================================================================
    val codecDef = '{
      new XmlCodec[T] {
        def encodeValue(in: T, out: XmlOutput): Unit = ${ Writer.genWriteVal(ctx, cfg, 'in, ref, 'out, false, false, '{}, '{}) }
        def decodeValue(in: XmlSource): T = ${ Reader.genReadVal(ctx, cfg, ref, 'in).asExprOf[T] }
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
    ).asExprOf[XmlCodec[T]]

    //    if ref.name.contains("Outer") then println(s"Codec: ${codec.show}")
    codec
