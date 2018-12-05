package co.blocke.scalajack
package parser1

case class JsonParserState(json: String) extends ParserState {

  type IN = String

  private var pos: Int = 0
  private val chars = json.toCharArray
  private val maxPos = chars.length

  private[parser1] def skipWhitespace() =
    while (pos < maxPos && chars(pos).isWhitespace)
      pos += 1

  @inline private[parser1] def skipInt() = {
    val mark = pos
    while (pos < maxPos && ((chars(pos) >= '0' && chars(pos) <= '9') || chars(pos) == '-' || chars(pos) == '+' || chars(pos) == 'e' || chars(pos) == 'E'))
      pos += 1
    json.substring(mark, pos)
  }

  @inline private[parser1] def skipTrue(): Boolean =
    if (pos + 3 < maxPos && json.substring(pos, pos + 4) == "true") {
      pos += 4
      true
    } else false

  @inline private[parser1] def skipFalse(): Boolean =
    if (pos + 4 < maxPos && json.substring(pos, pos + 5) == "false") {
      pos += 5
      true
    } else false

  @inline private[parser1] def char(): Char = chars(pos)
  @inline private[parser1] def advance(): Unit = pos += 1
}

case class JsonBooleanParser() extends BooleanParser {
  override def consumeBoolean(ps: ParserState): Boolean = {
    val jsps = ps.asInstanceOf[JsonParserState]
    jsps.skipWhitespace()
    if (jsps.skipTrue) {
      true
    } else if (jsps.skipFalse) {
      false
    } else super.consumeBoolean(ps)
  }
}

case class JsonIntParser() extends IntParser {
  override def consumeInt(ps: ParserState): BigInt = {
    val jsps = ps.asInstanceOf[JsonParserState]
    jsps.skipWhitespace()
    try {
      BigInt(jsps.skipInt())
    } catch {
      case t: Throwable => super.consumeInt(ps)
    }
  }
}

case class JsonArrayParser[E](elementTypeAdapter: TypeAdapter[E]) extends ArrayParser[E] {
  override def consumeArray(ps: ParserState): List[E] = {
    val jsps = ps.asInstanceOf[JsonParserState]
    jsps.skipWhitespace()
    if (jsps.char == '[')
      jsps.advance
    else
      throw new UnexpectedException(elementTypeAdapter, "Expected start of JSON array")

    val list = scala.collection.mutable.ListBuffer.empty[E]
    var index = 0
    while (jsps.char != ']') {
      list += elementTypeAdapter.parse(ps)
      jsps.char match {
        case ',' =>
          jsps.advance
          index += 1
        case ']' => // do nothing
        case x   => throw new UnexpectedException(elementTypeAdapter, s"Unexpected char $x in array", index)
      }
    }
    jsps.advance
    list.toList
  }
}

//
//trait ArrayParser[E, A <: Array[E], IN] extends Parser[IN] {
//  def consume(ps: ParserState[IN]) = consumeArray(ps)
