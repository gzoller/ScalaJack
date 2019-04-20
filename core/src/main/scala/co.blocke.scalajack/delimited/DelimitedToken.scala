package co.blocke.scalajack
package delimited

import model._
import TokenType._

case class DelimitedToken(input: String, tokenType: TokenType, begin: Int, end: Int) extends ParseToken[String] {
  def textValue: String = tokenType match {
    // $COVERAGE-OFF$Never called... here for match/case completeness
    case Null   => ""
    // $COVERAGE-ON$
    case String => input.substring(begin, end + 1)
    case QuotedString =>
      val text = input.substring(begin, end + 1)
      text.substring(1, text.length - 1).replaceAll("\"\"", "\"")
  }
}
