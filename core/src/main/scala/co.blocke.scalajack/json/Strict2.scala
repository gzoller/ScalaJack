package co.blocke.scalajack
package json

import java.util.ArrayList

import co.blocke.scalajack.util.Path
import model.Tokenizer
import model.TokenType._
import model.{ ReadUnexpectedError, Token, TokenType }

case class FunctionWithChain[A, B](main: A => Option[B], next: Option[FunctionWithChain[A, B]]) {
  def apply(arg: A): Option[B] = main(arg).orElse(next.map(_.apply(arg)).getOrElse(None))
}

case class StrictTokenizer2() extends Tokenizer[String] {

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

  val fail: FunctionWithChain[Int, Int] = FunctionWithChain[Int, Int](
    (i: Int) =>
      if (i == max)
        throw new ReadUnexpectedError(Path.Tokenizing, s"($i) Premature end of JSON.", List.empty[String])
      else if (expected == End)
        throw new ReadUnexpectedError(Path.Tokenizing, s"($i) Extra tokens past end of JSON.", List.empty[String])
      else
        throw new ReadUnexpectedError(Path.Tokenizing, s"($i) Expected a " + expected + " token here.", List(expected.toString)),
    None
  )

  val expectComma: FunctionWithChain[Int, Int] = FunctionWithChain[Int, Int](
    (i: Int) => if (i < max && chars(i) == ',') Some(i + 1) else None,
    Some(fail)
  )

  val expectColon: FunctionWithChain[Int, Int] = FunctionWithChain[Int, Int](
    (i: Int) => if (i < max && chars(i) == ':') Some(i + 1) else None,
    Some(fail)
  )

  val _strCheck: (Int) => Option[Int] =
    (i: Int) => {
      if (i < max && chars(i) == '"') {
        var j = i + 1
        while (j < max && chars(j) != '"') {
          if (chars(j) == '\\')
            j += 1
          j += 1
        }
        tokenspace.add(JsonToken(String, i + 1, j))
        Some(j + 1)
      } else
        None
    }

  val expectObject: FunctionWithChain[Int, Int] = FunctionWithChain[Int, Int](
    (i: Int) => {
      if (i < max && chars(i) == '{') {
        tokenspace.add(JsonToken(BeginObject, i, i))
        var j = i + 1
        if (chars(j) != '}') {
          expected = TokenType.String
          j = expectStringKey(eatWhitespace(j)).get
          expected = Colon
          j = expectColon(eatWhitespace(j)).get
          expected = JValue
          j = expectValue(eatWhitespace(j)).get
        }
        j = eatWhitespace(j)
        while (j < max && chars(j) != '}') {
          expected = Comma
          j = eatWhitespace(expectComma(j).get)
          expected = TokenType.String
          j = eatWhitespace(expectStringKey(j).get)
          expected = Colon
          j = eatWhitespace(expectColon(j).get)
          expected = JValue
          j = eatWhitespace(expectValue(j).get)
        }
        tokenspace.add(JsonToken(EndObject, j, j))
        Some(j + 1)
      } else
        None
    }, Some(fail))

  val expectArray: FunctionWithChain[Int, Int] = FunctionWithChain[Int, Int](
    (i: Int) => {
      if (i < max && chars(i) == '[') {
        tokenspace.add(JsonToken(BeginArray, i, i))
        var j = i + 1
        j = eatWhitespace(j)
        if (chars(j) != ']')
          j = expectValue(j).get
        j = eatWhitespace(j)
        while (j < max && chars(j) != ']') {
          expected = Comma
          j = eatWhitespace(expectComma(j).get)
          expected = JValue
          j = eatWhitespace(expectValue(j).get)
        }
        if (j == max)
          fail(j)
        else {
          tokenspace.add(JsonToken(EndArray, j, j))
          Some(j + 1)
        }
      } else
        None
    }, Some(expectObject))

  val expectStringKey: FunctionWithChain[Int, Int] = FunctionWithChain[Int, Int](_strCheck, Some(fail))

  val expectLiteral: FunctionWithChain[Int, Int] = FunctionWithChain[Int, Int](
    (i: Int) => {
      if (i < max && isNumberChar(chars(i))) {
        var j = i
        while (j < max && isNumberChar(chars(j)))
          j += 1
        tokenspace.add(JsonToken(Number, i, j))
        Some(j)
      } else if (i < max && source.length >= i + 4 && source.substring(i, i + 4) == "true") {
        tokenspace.add(JsonToken(True, i, i))
        Some(i + 4)
      } else if (i < max && source.length >= i + 5 && source.substring(i, i + 5) == "false") {
        tokenspace.add(JsonToken(False, i, i))
        Some(i + 5)
      } else if (i < max && source.length >= i + 4 && source.substring(i, i + 4) == "null") {
        tokenspace.add(JsonToken(Null, i, i))
        Some(i + 4)
      } else
        None
    }, Some(expectArray))

  // Top level Value/String... chained like this:  String -> Literal -> Array -> Object -> Fail
  val expectValue = FunctionWithChain[Int, Int](_strCheck, Some(expectLiteral))

  def tokenize(json: String): ArrayList[Token] = {
    source = json
    chars = json.toCharArray
    max = chars.length
    val i = expectValue(0).get
    if (i < max) {
      expected = End
      fail(i)
    }
    tokenspace.add(JsonToken(End, i, i))
    tokenspace
  }
}