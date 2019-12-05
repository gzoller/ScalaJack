package co.blocke.series60
package model

import java.util.ArrayList

/**
 * All the basic primitive token types ScalaJack handles.  These are a superset of the JSON tokens.
 */
object TokenType extends Enumeration {
  type TokenType = Value

  val BeginObject, EndObject, BeginArray, EndArray, Number, String, Boolean, Null, End, Colon, Comma, QuotedString = Value
}
import TokenType._

trait ParseToken[WIRE] {
  val input: WIRE
  val tokenType: TokenType
  def textValue: String
}

trait Tokenizer[WIRE] {
  def tokenize(source: WIRE): ArrayList[ParseToken[WIRE]]
}