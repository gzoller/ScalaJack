package co.blocke.scalajack
package json

import scala.quoted.*
import co.blocke.scala_reflection.{RTypeRef, TypedName}
import co.blocke.scala_reflection.reflect.rtypeRefs.*
import co.blocke.scala_reflection.reflect.ReflectOnType
import writing.*
import reading.JsonSource

object JsonCodecMaker:

  def generateCodecFor[T](ctx: CodecBuildContext, ref: RTypeRef[T], cfg: SJConfig)(using Type[T]): Expr[JsonCodec[T]] =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.*

    val typedName = ref.typedName

    // Make sure the writer is generated so it's in generatedWriters
    Writer.genWriterFor[T](ctx, cfg, '{ ??? : T }, ref, '{ ??? : JsonOutput })(using Type.of[T])

    // Build the codec definition using the writer method
    val codecExpr: Expr[JsonCodec[T]] = '{
      new JsonCodec[T] {
        def encodeValue(in: T, out: JsonOutput): Unit = {
          ${
            val typedName = ref.typedName
            ctx.generatedWriters.get(typedName) match
              case Some(writerExpr) =>
                '{ $writerExpr(in, out) }
              case None =>
                Writer.genWriterFor[T](ctx, cfg, 'in, ref, 'out)(using Type.of[T])
          }
        }
        def decodeValue(in: JsonSource): T = null.asInstanceOf[T]
      }
    }

    val retentionExprs: List[Statement] = ctx.generatedWriters.toList.map {
      case (_, writerExpr) =>
        // Call it with dummy args just to retain it in the tree
        val forced = '{ $writerExpr(null.asInstanceOf[Any], null.asInstanceOf[JsonOutput]) }
        forced.asTerm
    }
    // Emit println statements that call the writer methods to force retention
//    val retentionExprs: List[Statement] = ctx.generatedWriters.toList.map {
//      case (typedName, (sym, _)) =>
//        val paramTypes = sym.paramSymss.flatten.map(_.termRef.widenTermRefByName)
//        val dummyArgs = paramTypes.map {
//          case tpe =>
//            tpe.asType match
//              case '[tt] => '{ null.asInstanceOf[tt] }.asTerm
//        }
//        val callExpr = Apply(Ref(sym), dummyArgs).asExprOf[Unit]
//        '{ println("Retaining writer: " + ${Expr(typedName.toString)}); $callExpr }.asTerm
//    }

    val block = Block(
      retentionExprs,
      codecExpr.asTerm
    ).asExprOf[JsonCodec[T]]

    println("[🧩] Final codec tree:\n" + block.show)
    block
