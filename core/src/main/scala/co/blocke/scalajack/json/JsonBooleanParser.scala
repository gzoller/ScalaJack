package co.blocke.scalajack
package json

import model._
import org.json4s.{ JBool, JNull }

trait Json4sBooleanParser extends Parser {

  def parse[PARSER_STATE, AST](ps: PARSER_STATE)(implicit ops: Ops[AST]): AST = {
    val jsps = ps.asInstanceOf[JsonParserState]
    jsps.skipWhitespace()
    if (jsps.skipTrue)
      AstBoolean(true)
    else if (jsps.skipFalse)
      AstBoolean(false)
    else
      AstNull()
  }
}

case class JsonBooleanParser() extends Parser with Json4sBooleanParser

