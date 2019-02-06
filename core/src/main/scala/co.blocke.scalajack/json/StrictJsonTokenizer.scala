package co.blocke.scalajack
package json

import model._
import util.Path
import TokenType._
import java.util.ArrayList

case class StrictJsonTokenizer() extends Tokenizer[String] {

  @inline def isNumberChar(char: Char): Boolean =
    ('0' <= char && char <= '9') || (char == '-') || (char == '.') || (char == 'e') || (char == 'E') || (char == '-') || (char == '+')

  // Need this for all values that could potentially be in an Array
  //  @inline private def checkForArrayOrObject(exp: List[TokenType.Value]): List[TokenType.Value] =
  //    if (exp.head == EndArray)
  //      List(Comma, JValue) ++ exp
  //    else if (exp.head == EndObject)
  //      List(Colon, JValue, Comma) ++ exp
  //    else
  //      exp

  private def isExpected(token: TokenType.Value, exp: List[TokenType.Value], lastEmitted: Option[TokenType.Value]): Boolean =
    token match {
      //      case TokenType.String if (exp.head == JValue || exp.head == TokenType.String) => true
      //      case TokenType.Number if (exp.head == JValue)                                 => true
      //      case TokenType.True if (exp.head == JValue)                                   => true
      //      case TokenType.False if (exp.head == JValue)                                  => true
      //      case TokenType.Null if (exp.head == JValue)                                   => true
      //      case TokenType.BeginObject if (exp.head == JValue)                            => true
      //      case TokenType.BeginArray if (exp.head == JValue)                             => true

      //      case TokenType.EndArray if ((exp.size > 2 && exp.take(2) == List(Comma, EndArray))
      //        || (exp.size > 3 && exp.take(3) == List(JValue, Comma, EndArray)))
      //        && lastEmitted != Some(Comma) => true

      //      case TokenType.EndObject if ((exp.size > 2 && exp.take(2) == List(Comma, EndObject))
      //        || (exp.size > 5 && exp.take(5) == List(String, Colon, JValue, Comma, EndObject)))
      //        && lastEmitted != Some(Comma) => true

      //      case TokenType.Colon if (exp.head == Colon) => true
      //      case TokenType.Comma if (exp.head == Comma) => true
      case _ => false
    }

  private def nextExpected(justEmitted: TokenType.Value, exp: List[TokenType.Value]): List[TokenType.Value] =
    justEmitted match {
      //      case TokenType.String if(exp.head == JValue) => true
      //      case TokenType.Number if(exp.head == JValue) => true
      //      case TokenType.True if(exp.head == JValue) => true
      //      case TokenType.False if(exp.head == JValue) => true
      //      case TokenType.Null if(exp.head == JValue) => true
      case TokenType.BeginArray => List(JValue, Comma, EndArray) ++ exp.tail
      case TokenType.EndArray =>
        if (exp.head == JValue)
          exp.drop(3) // JValue,Comma,EndArray
        else
          exp.drop(2) // Comma,EndArray
      case TokenType.EndObject =>
        if (exp.head == String)
          exp.drop(5) // String,Colon,JValue,Comma,EndArray
        else
          exp.drop(2) // Comma,EndArray
      case TokenType.Comma =>
        if (exp.tail.head == EndArray)
          List(JValue, Comma) ++ exp.tail
        else if (exp.tail.head == EndObject)
          List(String, Colon, JValue, Comma) ++ exp.tail
        else
          throw new Exception("Boom")
      case TokenType.BeginObject => List(String, Colon, JValue, Comma, EndObject) ++ exp.tail
      case _ =>
        exp.tail
    }

  def tokenize(source: String): ArrayList[Token] = {
    val chars = source.toCharArray
    val max = chars.length
    var i: Int = 0
    val tokenspace = new ArrayList[Token]()
    var expected: List[TokenType.Value] = List(JValue, End)
    var lastEmitted: Option[TokenType.Value] = None

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
          if (!(expected.head == JValue || expected.head == TokenType.String))
            throw new ReadUnexpectedError(Path.Tokenizing, s"($i) Didn't expect a JSON string here", List(source.substring(mark, i), i.toString))
          tokenspace.add(JsonToken(String, mark, i))
          i += 1
          lastEmitted = Some(TokenType.String)
          expected = expected.tail

        case c if c.isWhitespace =>
          i += 1

        case '[' =>
          if (expected.head != JValue)
            throw new ReadUnexpectedError(Path.Tokenizing, s"($i) Didn't expect a JSON beginning of array here", List(i.toString))
          tokenspace.add(JsonToken(BeginArray, i, i))
          i += 1
          expected = nextExpected(TokenType.BeginArray, expected)

        case ']' =>
          if (((expected.size > 2 && expected.take(2) == List(Comma, EndArray))
            || (expected.size > 3 && expected.take(3) == List(JValue, Comma, EndArray)))
            && lastEmitted != Some(Comma)) {
            tokenspace.add(JsonToken(EndArray, i, i))
            i += 1
            lastEmitted = Some(JValue)
            expected = nextExpected(TokenType.EndArray, expected)
          } else
            throw new ReadUnexpectedError(Path.Tokenizing, s"($i) Didn't expect a JSON ending of array here", List(i.toString))

        case '{' =>
          if (expected.head != JValue)
            throw new ReadUnexpectedError(Path.Tokenizing, s"($i) Didn't expect a JSON beginning of object here", List(i.toString))
          tokenspace.add(JsonToken(BeginObject, i, i))
          i += 1
          expected = List(String, Colon, JValue, Comma, EndObject) ++ expected.tail

        case '}' =>
          if (((expected.size > 2 && expected.take(2) == List(Comma, EndObject))
            || (expected.size > 5 && expected.take(5) == List(String, Colon, JValue, Comma, EndObject)))
            && lastEmitted != Some(Comma)) {
            tokenspace.add(JsonToken(EndObject, i, i))
            i += 1
            lastEmitted = Some(JValue)
            expected = nextExpected(TokenType.EndObject, expected)
          } else
            throw new ReadUnexpectedError(Path.Tokenizing, s"($i) Didn't expect a JSON ending of object here", List(i.toString))

        case ':' =>
          if (expected.head != Colon)
            throw new ReadUnexpectedError(Path.Tokenizing, s"($i) Didn't expect a colon here", List(i.toString))
          i += 1
          lastEmitted = Some(Colon)
          expected = nextExpected(Colon, expected)

        case ',' =>
          if (expected.head != Comma)
            throw new ReadUnexpectedError(Path.Tokenizing, s"($i) Didn't expect a comma here", List(i.toString))
          i += 1
          lastEmitted = Some(Comma)
          expected = nextExpected(Comma, expected)

        case n if isNumberChar(n) =>
          val mark = i
          while (i < max && isNumberChar(chars(i)))
            i += 1
          if (expected.head != JValue)
            throw new ReadUnexpectedError(Path.Tokenizing, s"($mark) Didn't expect a JSON number here", List(source.substring(mark, i), i.toString))
          tokenspace.add(JsonToken(Number, mark, i))
          lastEmitted = Some(JValue)
          expected = expected.tail

        case 'n' =>
          if (source.length >= i + 4 && source.substring(i, i + 4) == "null") {
            if (expected.head != JValue)
              throw new ReadUnexpectedError(Path.Tokenizing, s"($i) Didn't expect a JSON null here", List(source.substring(i, i + 4), i.toString))
            tokenspace.add(JsonToken(Null, i, i))
            i += 4
            lastEmitted = Some(JValue)
            expected = expected.tail
          } else
            throw new ReadUnexpectedError(Path.Tokenizing, s"($i) Unexpected character 'n' at position $i", List(chars(i).toString, i.toString))

        case 't' =>
          if (source.length >= i + 4 && source.substring(i, i + 4) == "true") {
            if (expected.head != JValue)
              throw new ReadUnexpectedError(Path.Tokenizing, s"($i) Didn't expect a JSON true here", List(source.substring(i, i + 4), i.toString))
            tokenspace.add(JsonToken(True, i, i))
            i += 4
            lastEmitted = Some(JValue)
            expected = expected.tail
          } else
            throw new ReadUnexpectedError(Path.Tokenizing, s"($i) Unexpected character 't' at position $i", List(chars(i).toString, i.toString))

        case 'f' =>
          if (source.length >= i + 5 && source.substring(i, i + 5) == "false") {
            if (expected.head != JValue)
              throw new ReadUnexpectedError(Path.Tokenizing, s"($i) Didn't expect a JSON false here", List(source.substring(i, i + 5), i.toString))
            tokenspace.add(JsonToken(False, i, i))
            i += 5
            lastEmitted = Some(JValue)
            expected = expected.tail
          } else
            throw new ReadUnexpectedError(Path.Tokenizing, s"($i) Unexpected character 'f' at position $i", List(chars(i).toString, i.toString))

        case x =>
          throw new ReadUnexpectedError(Path.Tokenizing, s"($i) Unexpected character $x at position $i", List(x.toString, i.toString))
      }
    }
    if (expected.head != End)
      throw new ReadUnexpectedError(Path.Tokenizing, s"Premature end of JSON", List.empty[String])

    tokenspace.add(JsonToken(End, i, i))
    tokenspace
  }
}
