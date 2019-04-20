package co.blocke.scalajack
package json

import model._
import TokenType._

case class JsonToken(input: String, tokenType: TokenType, begin: Int, end: Int) extends ParseToken[String] {
  def textValue: String = input.substring(begin, end + 1)
}
