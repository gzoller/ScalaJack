package co.blocke.scalajack.json

import scala.quoted.*
import co.blocke.scala_reflection.RType

object JsonCodecMaker:
  inline def codecOf[T]: JsonCodec[T] = ${ generateCodecFor[T] }

  def generateCodecFor[T: Type](using Quotes): Expr[JsonCodec[T]] =
    val ctx = CodecBuildContext()
    val ref = co.blocke.scala_reflection.ReflectOnType[T]
    val writerExpr = Writer.genWriterFor[T](ctx, SJConfig.Default, '{ ??? }, ref, '{ ??? })
    '{
      new JsonCodec[T]:
        def encodeValue(in: T, out: JsonOutput): Unit =
          val asAny = in.asInstanceOf[Any]
          ${ writerExpr }
        def decodeValue(in: JsonSource): T = ???
    }
