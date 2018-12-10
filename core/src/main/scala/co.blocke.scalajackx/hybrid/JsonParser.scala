package co.blocke.scalajackx
package hybrid

import org.json4s._

trait Json4sParser {
  type AST = JValue
}

trait JsonIntParser extends Json4sParser with Parser {

  type T = BigInt

  override def parse(ps: ParserState): AST = {
    val jsps = ps.asInstanceOf[JsonParserState]
    jsps.skipWhitespace()
    JInt(BigInt(jsps.skipInt()))
  }

  override def toPrimitives(ast: AST): Any = ast.asInstanceOf[JInt].num
  override def fromPrimitives(prim: Any): AST = prim match {
    case b: BigInt => JInt(b)
    case _         => throw new Exception("Boom - from Int")
  }
}

trait JsonArrayParser[E] extends Json4sParser with ArrayParser[E] {

  type T = List[E]

  val elementTypeAdapter: TypeAdapter[E]

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
      list += elementTypeAdapter.serializer.parse(ps).asInstanceOf[AST]
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
  override def fromPrimitives(prim: Any): AST = prim match {
    case a: List[_] => JArray(a.map(e => elementTypeAdapter.serializer.fromPrimitives(e).asInstanceOf[AST]))
    case _          => throw new Exception("Boom - from Array")
  }
}