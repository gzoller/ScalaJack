package co.blocke.scalajack
package json

import model._
import org.json4s.JInt

trait Json4sIntParser extends Parser {

  def parse[PARSER_STATE, AST](ps: PARSER_STATE): AST = {
    val jsps = ps.asInstanceOf[JsonParserState]
    jsps.skipWhitespace()
    JInt(BigInt(jsps.skipInt())).asInstanceOf[AST]
  }
}

case class JsonIntParser() extends Json4sIntParser

