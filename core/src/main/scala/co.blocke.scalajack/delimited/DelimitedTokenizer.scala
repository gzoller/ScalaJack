package co.blocke.scalajack
package delimited

import java.util.ArrayList

import co.blocke.scalajack.model.ParseToken
import co.blocke.scalajack.model.TokenType._

case class DelimitedTokenizer(delimChar: Char) {

  def tokenize(source: String): ArrayList[ParseToken[String]] = {
    val chars = source.toCharArray
    val max = chars.length
    var i: Int = 0
    var start = 0
    val tokenspace = new ArrayList[ParseToken[String]]()
    var inQuotes = false

    while (i < max) {
      chars(i) match {
        case '"' if !inQuotes =>
          inQuotes = true
          if (i == 0 || (i > 0 && chars(i - 1) != '"'))
            start = i
          i += 1
        case '"' if (i < chars.length - 1 && chars(i + 1) == '"') =>
          i += 2
        case '"' =>
          inQuotes = false
          i += 1
        case `delimChar` if !inQuotes =>
          if (i == start)
            tokenspace.add(DelimitedToken(source, Null, start, start))
          else if (chars(start) == '"')
            tokenspace.add(DelimitedToken(source, QuotedString, start, i - 1))
          else
            tokenspace.add(DelimitedToken(source, String, start, i - 1))
          i += 1
          start = i
        case _ =>
          i += 1
      }
    }
    if (i != 0) {
      if (i == start)
        tokenspace.add(DelimitedToken(source, Null, start, start))
      else if (chars(start) == '"')
        tokenspace.add(DelimitedToken(source, QuotedString, start, i - 1))
      else
        tokenspace.add(DelimitedToken(source, String, start, i - 1))
    }
    tokenspace.add(DelimitedToken(source, End, i, i))
    tokenspace
  }
}
