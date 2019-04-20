package co.blocke.scalajack
package json

import model._
import util.Path
import TokenType._
import java.util.ArrayList

case class JsonTokenizer() extends Tokenizer[String] {

  @inline def isNumberChar(char: Char): Boolean =
    ('0' <= char && char <= '9') || (char == '-') || (char == '.') || (char == 'e') || (char == 'E') || (char == '-') || (char == '+')

  def tokenize(source: String): ArrayList[ParseToken[String]] = {
    val chars = source.toCharArray
    val max = chars.length
    var i: Int = 0
    val tokenspace = new ArrayList[ParseToken[String]]()

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
          tokenspace.add(JsonToken(source, String, mark, i - 1))
          i += 1
        case c if c.isWhitespace =>
          i += 1
        case '[' =>
          tokenspace.add(JsonToken(source, BeginArray, i, i))
          i += 1
        case ']' =>
          tokenspace.add(JsonToken(source, EndArray, i, i))
          i += 1
        case '{' =>
          tokenspace.add(JsonToken(source, BeginObject, i, i))
          i += 1
        case '}' =>
          tokenspace.add(JsonToken(source, EndObject, i, i))
          i += 1
        case ':' =>
          tokenspace.add(JsonToken(source, Colon, i, i))
          i += 1
        case ',' =>
          tokenspace.add(JsonToken(source, Comma, i, i))
          i += 1
        case n if isNumberChar(n) =>
          val mark = i
          while (i < max && isNumberChar(chars(i)))
            i += 1
          tokenspace.add(JsonToken(source, Number, mark, i - 1))
        case 'n' =>
          if (source.length >= i + 4 && source.substring(i, i + 4) == "null") {
            tokenspace.add(JsonToken(source, Null, i, i + 3))
            i += 4
          } else
            throw new ReadUnexpectedError(s"[${Path.Tokenizing.toString}]: Unexpected character 'n' at position $i\n" + showError(i, source))
        case 't' =>
          if (source.length >= i + 4 && source.substring(i, i + 4) == "true") {
            tokenspace.add(JsonToken(source, Boolean, i, i + 3))
            i += 4
          } else
            throw new ReadUnexpectedError(s"[${Path.Tokenizing.toString}]: Unexpected character 't' at position $i\n" + showError(i, source))
        case 'f' =>
          if (source.length >= i + 5 && source.substring(i, i + 5) == "false") {
            tokenspace.add(JsonToken(source, Boolean, i, i + 4))
            i += 5
          } else
            throw new ReadUnexpectedError(s"[${Path.Tokenizing.toString}]: Unexpected character 'f' at position $i\n" + showError(i, source))
        case x =>
          throw new ReadUnexpectedError(s"[${Path.Tokenizing.toString}]: Unexpected character $x at position $i\n" + showError(i, source))
      }
    }
    tokenspace.add(JsonToken(source, End, i, i))
    tokenspace
  }

  def showError(charPos: Int, json: String): String = {
    val startPosOffset = if (charPos - 50 < 0) charPos else 50
    val startPos = charPos - startPosOffset
    val endPos = if (charPos + 50 > json.length) json.length else charPos + 50
    val buf = new StringBuffer()
    buf.append(json.subSequence(startPos, endPos).toString + "\n")
    val line = json.subSequence(startPos, startPos + startPosOffset).toString.map(_ match {
      case '\n' => '\n'
      case _    => '-'
    }).mkString + "^"
    buf.append(line)
    buf.toString
  }
}
