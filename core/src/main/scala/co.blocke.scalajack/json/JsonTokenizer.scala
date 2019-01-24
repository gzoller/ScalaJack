package co.blocke.scalajack
package json

import model._
import util.Path
import TokenType._
import java.util.ArrayList

case class JsonTokenizer() extends Tokenizer[String] {

  @inline def isNumberChar(char: Char): Boolean =
    ('0' <= char && char <= '9') || (char == '-') || (char == '.') || (char == 'e') || (char == 'E') || (char == '-') || (char == '+')

  def tokenize(source: String): ArrayList[Token] = {
    val chars = source.toCharArray
    val max = chars.length
    var i: Int = 0
    val tokenspace = new ArrayList[Token]()

    while (i < max) {
      chars(i) match {
        case '"' =>
          i += 1
          val mark = i
          while (i < max && chars(i) != '"') {
            if (chars(i) == '\\')
              i += 1
            i += 1
          }
          tokenspace.add(JsonToken(String, mark, i))
          i += 1
        case c if c.isWhitespace =>
          i += 1
        case '[' =>
          tokenspace.add(JsonToken(BeginArray, i, i))
          i += 1
        case ']' =>
          tokenspace.add(JsonToken(EndArray, i, i))
          i += 1
        case '{' =>
          tokenspace.add(JsonToken(BeginObject, i, i))
          i += 1
        case '}' =>
          tokenspace.add(JsonToken(EndObject, i, i))
          i += 1
        case ':' =>
          i += 1
        case ',' =>
          i += 1
        case n if isNumberChar(n) =>
          val mark = i
          while (i < max && isNumberChar(chars(i)))
            i += 1
          tokenspace.add(JsonToken(Number, mark, i))
        case 'n' =>
          tokenspace.add(JsonToken(Null, i, i))
          i += 4
        case 't' =>
          tokenspace.add(JsonToken(True, i, i))
          i += 4
        case 'f' =>
          tokenspace.add(JsonToken(False, i, i))
          i += 5
        case x =>
          throw new ReadUnexpectedError(Path.Tokenizing, s"Unexpected character $x at position $i", List(x.toString, i.toString))
      }
    }
    tokenspace.add(JsonToken(End, i, i))
    tokenspace
  }
}
