package co.blocke.scalajackx
package json

import org.json4s.{ JBool, JNull }

case class JsonBooleanSerializer() extends JsonSerializer {

  override def _parse(ps: ParserState): AST = {
    val jsps = ps.asInstanceOf[JsonParserState]
    jsps.skipWhitespace()
    if (jsps.skipTrue) {
      JBool(true)
    } else if (jsps.skipFalse) {
      JBool(false)
    } else JNull
  }
  override def emit(ast: AST): String = ast.asInstanceOf[JBool].value.toString
}
