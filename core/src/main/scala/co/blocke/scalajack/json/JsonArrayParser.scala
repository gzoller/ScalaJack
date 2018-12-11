package co.blocke.scalajack
package json

import model._
import org.json4s.{ JArray, JValue }

trait Json4sArrayParser[E] extends ArrayParser[E] {

  val elementTypeAdapter: TypeAdapter[E]

  def parse[PARSER_STATE, AST](ps: PARSER_STATE)(implicit ops: Ops[AST]): AST = {
    val jsps = ps.asInstanceOf[JsonParserState]
    jsps.skipWhitespace()
    if (jsps.char == '[')
      jsps.advance
    else
      throw new UnexpectedException(elementTypeAdapter, "Expected start of JSON array")

    val list = scala.collection.mutable.ListBuffer.empty[AST]
    var index = 0
    while (jsps.char != ']') {
      list += elementTypeAdapter.parser.parse(ps)
      jsps.char match {
        case ',' =>
          jsps.advance
          index += 1
        case ']' => // do nothing
        case x   => throw new UnexpectedException(elementTypeAdapter, s"Unexpected char $x in array", index)
      }
    }
    jsps.advance
    AstArray(list.toList)
  }
}

case class JsonArrayParser[E](elementTypeAdapter: TypeAdapter[E])(implicit tt: TypeTag[E]) extends Json4sArrayParser[E]
