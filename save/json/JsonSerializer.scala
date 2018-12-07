package co.blocke.scalajackx
package json

import org.json4s.JValue

trait JsonSerializer extends Serializer {

  type AST = JValue
  type WIRE = String

  val ops = new JsonAstOps()

  protected def createParserState(wire: WIRE): ParserState = JsonParserState(wire)
}
