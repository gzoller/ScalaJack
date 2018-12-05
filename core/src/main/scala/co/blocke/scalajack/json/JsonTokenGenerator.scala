package co.blocke.scalajack
package json

import parser._

case class JsonParserState(json: String) extends ParserState[String] {
  private[json] var pos: Int = 0
  private val chars = json.toCharArray
  private val maxPos = chars.length

  private[json] def skipWhitespace() =
    while (pos < maxPos && chars(pos).isWhitespace)
      pos += 1

  @inline private[json] def skipInt() =
    while (pos < maxPos && ((chars(pos) >= '0' && chars(pos) <= '9') || chars(pos) == '-' || chars(pos) == '+' || chars(pos) == 'e' || chars(pos) == 'E'))
      pos += 1

  @inline private[json] def skipTrue(): Boolean =
    if (pos + 3 < maxPos && json.substring(pos, pos + 4) == "true") {
      pos += 4
      true
    } else false

  @inline private[json] def skipFalse(): Boolean =
    if (pos + 4 < maxPos && json.substring(pos, pos + 5) == "false") {
      pos += 5
      true
    } else false
}

case class JsonBooleanParser() extends SimpleParser[String] {
  override def consumeBoolean(ps: ParserState[String]): Boolean = {
    val jsps = ps.asInstanceOf[JsonParserState]
    jsps.skipWhitespace()
    if (jsps.skipTrue) {
      true
    } else if (jsps.skipFalse) {
      false
    } else super.consumeBoolean(ps)
  }
}

case class JsonIntParser() extends SimpleParser[String] {
  override def consume[BigInt](ps: ParserState[String]): BigInt = {
    //  override def consumeInt(ps: ParserState[String]): BigInt = {
    val jsps = ps.asInstanceOf[JsonParserState]
    jsps.skipWhitespace()
    val mark = jsps.pos
    jsps.skipInt()
    try {
      BigInt(jsps.json.substring(mark, jsps.pos))
    } catch {
      case t: Throwable => super.consumeInt(ps)
    }
  }
}

//case class JsonArrayParser[E, A <: Array[E]](elementPath: Path[E]) extends ArrayParser[E,A,String] {
//  override def consume(ps: ParserState[String]): A = ???
//}
//
//trait ArrayParser[E, A <: Array[E], IN] extends Parser[IN] {
//  def consume(ps: ParserState[IN]) = consumeArray(ps)
