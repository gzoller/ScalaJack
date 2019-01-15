package co.blocke.scalajack
package model

//import collection.mutable.ArrayBuffer
import java.util.ArrayList

object TokenType extends Enumeration {
  type TokenType = Value

  val BeginObject, EndObject, BeginArray, EndArray, Number, String, True, False, Null, End = Value
}

import TokenType._

trait Token {
  val tokenType: TokenType
}

trait Tokenizer[WIRE] {
  def tokenize(source: WIRE): ArrayList[Token] //ArrayBuffer[Token]
}