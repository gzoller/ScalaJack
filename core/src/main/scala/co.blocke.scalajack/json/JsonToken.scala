package co.blocke.scalajack
package json

import model._
import TokenType._

case class JsonToken(tokenType: TokenType, begin: Int, end: Int) extends Token
