package co.blocke.scalajack
package json

// $COVERAGE-OFF$Demonstration only

import java.util.ArrayList

import model.Tokenizer
import model.TokenType._
import model.{ Token, TokenType }

/**
 * This class is included for demonstration purposes only!
 * It is a decent example of the practical use of composition of partial functions.
 * In practice, though, it is slower than a "flat" tokenizer, so it is unused in ScalaJack.
 */

case class StrictTokenizer() extends Tokenizer[String] {

  type expectFn = (Int) => (Boolean, Int)

  @inline def isNumberChar(char: Char): Boolean =
    ('0' <= char && char <= '9') || (char == '-') || (char == '.') || (char == 'e') || (char == 'E') || (char == '-') || (char == '+')

  @inline def eatWhitespace(p: Int): Int = {
    var j = p
    while (j < max && chars(j).isWhitespace)
      j += 1
    j
  }

  val tokenspace = new ArrayList[Token]()
  var source: String = null
  var chars: Array[Char] = null
  var max: Int = 0
  var expected: TokenType.Value = JValue

  val expectComma: PartialFunction[Int, Int] = {
    case i if i < max && chars(i) == ',' => i + 1
  }

  val expectColon: PartialFunction[Int, Int] = {
    case i if i < max && chars(i) == ':' => i + 1
  }

  val expectString: PartialFunction[Int, Int] = {
    case i if i < max && chars(i) == '"' =>
      var j = i + 1
      while (j < max && chars(j) != '"') {
        if (chars(j) == '\\')
          j += 1
        j += 1
      }
      tokenspace.add(JsonToken(String, i + 1, j))
      j + 1
  }

  val expectLiteral: PartialFunction[Int, Int] = {
    case i if i < max && isNumberChar(chars(i)) =>
      var j = i
      while (j < max && isNumberChar(chars(j)))
        j += 1
      tokenspace.add(JsonToken(Number, i, j))
      j
    case i if i < max && source.length >= i + 4 && source.substring(i, i + 4) == "true" =>
      tokenspace.add(JsonToken(True, i, i))
      i + 4
    case i if i < max && source.length >= i + 5 && source.substring(i, i + 5) == "false" =>
      tokenspace.add(JsonToken(False, i, i))
      i + 5
    case i if i < max && source.length >= i + 4 && source.substring(i, i + 4) == "null" =>
      tokenspace.add(JsonToken(Null, i, i))
      i + 4
  }

  val expectArray: PartialFunction[Int, Int] = {
    case i if i < max && chars(i) == '[' =>
      tokenspace.add(JsonToken(BeginArray, i, i))
      var j = i + 1
      j = eatWhitespace(j)
      if (chars(j) != ']')
        j = expectValue(j)
      j = eatWhitespace(j)
      while (j < max && chars(j) != ']') {
        expected = Comma
        j = eatWhitespace((expectComma orElse fail)(j))
        expected = JValue
        j = eatWhitespace((expectValue orElse fail)(j))
      }
      if (j == max)
        fail(j)
      else {
        tokenspace.add(JsonToken(EndArray, j, j))
        j + 1
      }
  }

  val expectObject: PartialFunction[Int, Int] = {
    case i if i < max && chars(i) == '{' =>
      tokenspace.add(JsonToken(BeginObject, i, i))
      var j = i + 1
      if (chars(j) != '}') {
        expected = TokenType.String
        j = (expectString orElse fail)(eatWhitespace(j))
        expected = Colon
        j = (expectColon orElse fail)(eatWhitespace(j))
        expected = JValue
        j = (expectValue orElse fail)(eatWhitespace(j))
      }
      j = eatWhitespace(j)
      while (j < max && chars(j) != '}') {
        expected = Comma
        j = eatWhitespace((expectComma orElse fail)(j))
        expected = TokenType.String
        j = eatWhitespace((expectString orElse fail)(j))
        expected = Colon
        j = eatWhitespace((expectColon orElse fail)(j))
        expected = JValue
        j = eatWhitespace((expectValue orElse fail)(j))
      }
      tokenspace.add(JsonToken(EndObject, j, j))
      j + 1
  }

  val fail: PartialFunction[Int, Int] = {
    case i if i == max        => throw new Exception(s"($i) Premature end of JSON.")
    case i if expected == End => throw new Exception(s"($i) Extra tokens past end of JSON.")
    case i                    => throw new Exception(s"($i) Boom! Expected a " + expected + " token here.")
  }

  val expectValue: PartialFunction[Int, Int] = {
    case i if i < max =>
      expected = JValue
      (expectString orElse expectLiteral orElse expectArray orElse expectObject orElse fail)(eatWhitespace(i))
  }

  def tokenize(json: String): ArrayList[Token] = {
    source = json
    chars = json.toCharArray
    max = chars.length
    val i = expectValue(0)
    if (i < max) {
      expected = End
      fail(i)
    }
    tokenspace.add(JsonToken(End, i, i))
    tokenspace
  }

}
// $COVERAGE-ON$