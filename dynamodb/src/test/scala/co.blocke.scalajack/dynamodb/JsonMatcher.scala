package co.blocke.scalajack
package dynamodb

import co.blocke.scalajack.json.JSON
import org.json4s.JsonAST.JValue
import org.json4s.native.JsonMethods._
import org.json4s.string2JsonInput

object JsonMatcher {

  def jsonMatches( expected: JSON, actual: JSON ): Boolean = 
    val diffs = JsonDiff.compare(
      parseJValue(expected.asInstanceOf[String]),
      parseJValue(actual.asInstanceOf[String]),
      "expected",
      "actual"
    )
    diffs.isEmpty

  implicit def parseJValue(string: String): JValue = parse(string)
}
