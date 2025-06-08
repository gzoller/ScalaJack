package co.blocke.scalajack
package json
package writing

import co.blocke.scala_reflection.RType
import co.blocke.scala_reflection.rtypes.*
import shared.CodecBuildContext

import org.apache.commons.text.StringEscapeUtils
import scala.annotation.tailrec
import scala.util.*
import scala.quoted.*

/** Writing an Any-typed value is a hot mess.  You don't know until runtime exactly what the target object will actually
  * be, so all the compile-time macro magic will be useless.  It is 100% runtime, so it will be much slower as well.
  * Basically straightforward things (which is 80-90% of what people do) should work fine.  If you have a case that
  * misbehaves, my best advice is to use a more specific type.  As a rule, it's a much better design choice anyway!
  */

object AnyWriter:

  def writeAny(cfg: SJConfig, target: Any, out: JsonOutput, inTuple: Boolean = false): Unit =
    // val rt = RType.of(target.getClass)
    target match
      case null                        => out.burpNull()
      case v: BigDecimal               => out.value(v)
      case v: BigInt                   => out.value(v)
      case v: Boolean                  => out.value(v)
      case v: Byte                     => out.value(v)
      case v: Char                     => out.value(v)
      case v: Double                   => out.value(v)
      case v: Float                    => out.value(v)
      case v: Int                      => out.value(v)
      case v: Long                     => out.value(v)
      case v: Short                    => out.value(v)
      case v: String                   => out.value(StringEscapeUtils.escapeJson(v))
      case v: java.lang.Boolean        => out.value(v)
      case v: java.lang.Byte           => out.value(v)
      case v: java.lang.Character      => out.value(v)
      case v: java.lang.Double         => out.value(v)
      case v: java.lang.Float          => out.value(v)
      case v: java.lang.Integer        => out.value(v)
      case v: java.lang.Long           => out.value(v)
      case v: java.lang.Short          => out.value(v)
      case v: java.lang.Number         => out.value(v)
      case v: java.time.Duration       => out.value(v)
      case v: java.time.Instant        => out.value(v)
      case v: java.time.LocalDate      => out.value(v)
      case v: java.time.LocalDateTime  => out.value(v)
      case v: java.time.LocalTime      => out.value(v)
      case v: java.time.MonthDay       => out.value(v)
      case v: java.time.OffsetDateTime => out.value(v)
      case v: java.time.OffsetTime     => out.value(v)
      case v: java.time.Period         => out.value(v)
      case v: java.time.Year           => out.value(v)
      case v: java.time.YearMonth      => out.value(v)
      case v: java.time.ZoneOffset     => out.value(v)
      case v: java.time.ZonedDateTime  => out.value(v)
      case v: java.time.ZoneId         => out.value(v)
      case v: java.util.UUID           => out.value(v)

      case v: Array[?] =>
        out.startArray()
        v.foreach(e => writeAny(cfg, e, out))
        out.endArray()

      case v: Set[?] =>
        out.startArray()
        v.foreach(e => writeAny(cfg, e, out))
        out.endArray()

      case v: Seq[?] =>
        out.startArray()
        v.foreach(e => writeAny(cfg, e, out))
        out.endArray()

      case v: Map[?, ?] =>
        out.startObject()
        v.foreach { case (k, v) => _okToWrite(cfg, k.toString, v, out) }
        out.endObject()

      case v: Option[?] =>
        v match
          case None =>
            if inTuple then out.burpNull()
            else ()
          case Some(v2) => writeAny(cfg, v2, out)

      case v: Either[?, ?] =>
        v match
          case Left(_) =>
            if inTuple then out.burpNull()
            else ()
          case Right(v2) => writeAny(cfg, v2, out)

      case v: Try[?] =>
        v match
          case Failure(_) =>
            if inTuple then out.burpNull()
            else ()
          case Success(v2) => writeAny(cfg, v2, out)

      case v: Tuple =>
        val varr = v.toArray
        out.startArray()
        varr.foreach(v2 => writeAny(cfg, v2, out, true))
        out.endArray()

      case v =>
        val rt = RType.of(v.getClass)
        rt match
          case t: ScalaClassRType[?] =>
            out.startObject()
            t.fields.foreach(f =>
              val field = f.asInstanceOf[ScalaFieldInfo]
              val m = v.getClass.getMethod(field.name)
              m.setAccessible(true)
              val fieldValue = m.invoke(v)
              val fieldName = f.annotations.get("co.blocke.scalajack.jsLabel").flatMap(_.get("name")).getOrElse(f.name)
              _okToWrite(cfg, fieldName, fieldValue, out)
            )
            out.endObject()
          case _ => throw new UnsupportedType("Class " + v.getClass.getName + " not supported for Any type")

  // Called by non-Any classes (in JsonCodecMaker) that have Any-typed fields
  def isOkToWrite(ctx: CodecBuildContext, cfg: SJConfig, prefix: Expr[Unit], value: Expr[Any], out: Expr[JsonOutput]): Expr[Unit] =
    given Quotes = ctx.quotes
    '{
      val _ = _okToWrite(${ Expr(cfg) }, $value).map { v =>
        $prefix
        AnyWriter.writeAny(${ Expr(cfg) }, v, $out)
      }
    }

  // Called for Any-typed classes
  private def _okToWrite(cfg: SJConfig, label: String, value: Any, out: JsonOutput): Unit =
    _okToWrite(cfg, value).foreach { v =>
      out.label(label)
      AnyWriter.writeAny(cfg, v, out)
    }

  @tailrec
  private def _okToWrite(cfg: SJConfig, value: Any): Option[Any] =
    value match
      case None => if cfg.noneAsNull then Some(null) else None
      case Failure(e) =>
        cfg.tryFailureHandling match
          case TryPolicy.AS_NULL         => Some(null)
          case TryPolicy.ERR_MSG_STRING  => Some("Try Failure with msg: " + e.getMessage)
          case TryPolicy.THROW_EXCEPTION => throw e

      case Left(v) =>
        cfg.eitherLeftHandling match
          case EitherLeftPolicy.AS_VALUE        => Some(v)
          case EitherLeftPolicy.AS_NULL         => Some("null")
          case EitherLeftPolicy.ERR_MSG_STRING  => Some("Left Error: " + v.toString)
          case EitherLeftPolicy.THROW_EXCEPTION => throw new EitherLeftError("Left Error: " + v.toString)
      case Some(v) => _okToWrite(cfg, v)
      case _       => Some(value)
