package co.blocke.scalajack
package json

import model._
import TokenType._
import collection.mutable.ArrayBuffer

case class JsonTokenizer() extends Tokenizer[String] {

  @inline def isNumberChar(char: Char): Boolean =
    ('0' <= char && char <= '9') || (char == '-') || (char == '.') || (char == 'e') || (char == 'E') || (char == '-') || (char == '+')

  def tokenize(source: String): ArrayBuffer[Token] = {
    try {
      val chars = source.toCharArray
      val max = chars.length
      var i: Int = 0
      var p: Int = 0
      val tokenspace = new ArrayBuffer[Token](3000)

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
            tokenspace += JsonToken(String, mark, i)
            i += 1
          case c if c.isWhitespace =>
            i += 1
          case '[' =>
            tokenspace += JsonToken(BeginArray, i, i)
            i += 1
          case ']' =>
            tokenspace += JsonToken(EndArray, i, i)
            i += 1
          case '{' =>
            tokenspace += JsonToken(BeginObject, i, i)
            i += 1
          case '}' =>
            tokenspace += JsonToken(EndObject, i, i)
            i += 1
          case ':' =>
            i += 1
          case ',' =>
            i += 1
          case n if isNumberChar(n) =>
            val mark = i
            while (i < max && isNumberChar(chars(i)))
              i += 1
            tokenspace += JsonToken(Number, mark, i)
          case 'n' =>
            tokenspace += JsonToken(Null, i, i)
            i += 4
          case 't' =>
            tokenspace += JsonToken(True, i, i)
            i += 4
          case 'f' =>
            tokenspace += JsonToken(False, i, i)
            i += 5
          case x =>
            throw new Exception(s"Boom... unexpected character $x at position $i")
        }
      }
      tokenspace += JsonToken(End, i, i)
    }
  }
}
