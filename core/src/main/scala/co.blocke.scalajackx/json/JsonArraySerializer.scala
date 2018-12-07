package co.blocke.scalajackx
package json

import util.UnexpectedException
import org.json4s.JArray

case class JsonArraySerializer[E](elementTypeAdapter: TypeAdapter[E]) extends JsonSerializer with ArraySerializer[E] {

  override def _parse(ps: ParserState): AST = {
    val jsps = ps.asInstanceOf[JsonParserState]
    jsps.skipWhitespace()
    if (jsps.char == '[')
      jsps.advance
    else
      throw new UnexpectedException(elementTypeAdapter, "Expected start of JSON array")

    val list = scala.collection.mutable.ListBuffer.empty[AST]
    var index = 0
    while (jsps.char != ']') {
      list += elementTypeAdapter.serializer._parse(ps).asInstanceOf[AST]
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

  override def emit(ast: AST): String =
    ast.asInstanceOf[JArray].arr.map( e => elementTypeAdapter.serializer.emit(e.asInstanceOf[elementTypeAdapter.serializer.AST]) ).mkString("[",",","]")
}
