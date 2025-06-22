package co.blocke.scalajack

import co.blocke.scalajack.json.reading

import scala.util.control.NoStackTrace

class IllegalKeyType(msg: String) extends Throwable(msg) with NoStackTrace
class NullKeyValue(msg: String) extends Throwable(msg) with NoStackTrace
class UnsupportedType(msg: String) extends Throwable(msg) with NoStackTrace
class EitherLeftError(msg: String) extends Throwable(msg) with NoStackTrace
class IllegalCharacterError(msg: String) extends Throwable(msg) with NoStackTrace

class ParseError(val msg: String) extends Throwable(msg) with NoStackTrace:
  val show: String = msg

// Thrown at compile-time only!
case class TypeError(override val msg: String) extends ParseError(msg) with NoStackTrace:
  override val show: String = ""

// Thrown at runtime only!
case class JsonParseError(override val msg: String, context: reading.JsonSource) extends ParseError(msg + " at position " + context.pos) with NoStackTrace:
  override val show: String =
    val js = context.js.toString
    val (clip, dashes) = context.pos match {
      case ep if ep <= 50 && context.max < 80 => (js, ep)
      case ep if ep <= 50                     => (js.substring(0, 77) + "...", ep)
      case ep if ep > 50 && ep + 30 >= context.max =>
        ("..." + js.substring(context.pos - 49), 52)
      case ep => ("..." + js.substring(ep - 49, ep + 27) + "...", 52)
    }
    msg + s" at position [${context.pos}]" + "\n" + clip.replaceAll("[\n\t]", "~") + "\n" + ("-" * dashes) + "^"
