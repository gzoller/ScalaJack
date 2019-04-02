package co.blocke.scalajack
package delimited

import java.util.ArrayList

import co.blocke.scalajack.json.JsonToken
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
            start = i + 1 // skip quote char
          i += 1
        case '"' =>
          inQuotes = false
          i += 1
        case `delimChar` if !inQuotes =>
          var end = i - 1
          if (i - 1 >= 0 && chars(end) == '"')
            end -= 1 // don't include quotes
          if (end < start)
            tokenspace.add(JsonToken(source, Null, start, start))
          else
            tokenspace.add(JsonToken(source, String, start, end))
          i += 1
          start = i
        case _ =>
          i += 1
      }
    }
    if (i != 0) {
      var end = i - 1
      if (i - 1 >= 0 && chars(end) == '"')
        end -= 1 // don't include quotes
      if (end < start)
        tokenspace.add(JsonToken(source, Null, start, start))
      else
        tokenspace.add(JsonToken(source, String, start, end))
    }
    tokenspace.add(JsonToken(source, End, i, i))
    tokenspace

    // end < start means empty field
  }
}
