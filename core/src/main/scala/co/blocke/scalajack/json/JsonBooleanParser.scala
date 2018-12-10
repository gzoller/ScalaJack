package co.blocke.scalajack
package json

import model.Parser
import org.json4s.{ JBool, JNull }

trait Json4sBooleanParser extends Parser {

  def parse[PARSER_STATE, AST](ps: PARSER_STATE): AST = {
    val jsps = ps.asInstanceOf[JsonParserState]
    jsps.skipWhitespace()
    if (jsps.skipTrue)
      JBool(true).asInstanceOf[AST]
    else if (jsps.skipFalse)
      JBool(false).asInstanceOf[AST]
    else
      JNull.asInstanceOf[AST]
  }
}

case class JsonBooleanParser() extends Parser with Json4sBooleanParser

