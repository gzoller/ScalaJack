package co.blocke.scalajack
package json

import parser._

case class JsonParserState(json: String) extends ParserState {
  private[json] var pos: Int = 0
  private val chars = json.toCharArray
  private val maxPos = chars.length
  private var savedPos = -1

  private[json] def skipWhitespace() =
    while( pos < maxPos && chars(pos).isWhitespace )
      pos += 1

  private[json] def skipInt() =
    while( pos < maxPos && chars(pos).is )
      pos += 1

  //  private[json] def savePos(): Unit =
//    savedPos = pos
//  private[json] def restorePos(): Unit =
//    if( savedPos >= 0 )
//      pos = savedPos
}

case class JsonBooleanParser() extends BooleanParser {
  protected[parser] override def consume( ps: ParserState ): Boolean = {
    val jsps = ps.asInstanceOf[JsonParserState]
    jsps.skipWhitespace()
    if (jsps.json.substring(jsps.pos, 4) == "true") {
      jsps.pos += 4
      true
    }
    else if (jsps.json.substring(jsps.pos, 5) == "false") {
      jsps.pos += 5
      false
    }
    else super.consume(ps)
  }
}


case class IntBooleanParser() extends IntParser {
  protected[parser] override def consume( ps: ParserState ): Boolean = {
    val jsps = ps.asInstanceOf[JsonParserState]
    jsps.skipWhitespace()
    val mark = jsps.pos
    if (jsps.json.substring(jsps.pos, 4) == "true") {
      jsps.pos += 4
      true
    }
    else if (jsps.json.substring(jsps.pos, 5) == "false") {
      jsps.pos += 5
      false
    }
    else super.consume(ps)
  }
}