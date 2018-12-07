package co.blocke.scalajackx
package hybrid

import org.json4s._

trait JsonParser {
  type AST = JValue
}

case class JsonIntParser() extends JsonParser with Parser {

  type T = BigInt

  override def parse(ps: ParserState): AST = {
    val jsps = ps.asInstanceOf[JsonParserState]
    jsps.skipWhitespace()
    JInt(BigInt(jsps.skipInt()))
  }

  override def toPrimitives(ast: AST): Any = ast.asInstanceOf[JInt].num
}

case class JsonArrayParser[E](elementTypeAdapter: TypeAdapter[E]) extends JsonParser with ArrayParser[E] {

  type T = List[E]

  override def parse(ps: ParserState): AST = {
    val jsps = ps.asInstanceOf[JsonParserState]
    jsps.skipWhitespace()
    if (jsps.char == '[')
      jsps.advance
    else
      throw new UnexpectedException(elementTypeAdapter, "Expected start of JSON array")

    val list = scala.collection.mutable.ListBuffer.empty[AST]
    var index = 0
    while (jsps.char != ']') {
      list += elementTypeAdapter.parser.parse(ps).asInstanceOf[AST]
      jsps.char match {
        case ',' =>
          jsps.advance
          index += 1
        case ']' => // do nothing
        case x   => throw new UnexpectedException(elementTypeAdapter, s"Unexpected char $x in array", index)
      }
    }
    jsps.advance
    JArray(list.toList)
  }

  override def toPrimitives(ast: AST): Any = ast.values
}