package co.blocke.scalajack
package json4s

import model.ParseToken
import model.TokenType._
import org.json4s.JValue

case class JValueToken(input: JValue, tokenType: TokenType) extends ParseToken[JValue] {
  def textValue: String = input.values.toString
}