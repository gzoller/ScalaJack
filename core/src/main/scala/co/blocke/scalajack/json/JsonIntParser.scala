package co.blocke.scalajack
package json

import model._

trait Json4sIntParser extends Parser {

  def parse[PARSER_STATE, AST](ps: PARSER_STATE)(implicit ops: Ops[AST]): AST = {
    val jsps = ps.asInstanceOf[JsonParserState]
    jsps.skipWhitespace()
    AstInt(BigInt(jsps.skipInt()))
  }
}

case class JsonIntParser() extends Json4sIntParser

