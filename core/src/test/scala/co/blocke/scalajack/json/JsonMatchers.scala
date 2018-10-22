package co.blocke.scalajack
package json

import org.json4s.JsonAST.JValue
import org.json4s.native.JsonMethods._
import org.scalatest.matchers.{ MatchResult, Matcher }

object JsonMatchers {

  implicit def parseJValue(string: String): JValue = parse(string)

  def matchJson(expected: JValue): Matcher[JValue] = new Matcher[JValue] {

    override def apply(actual: JValue): MatchResult = {
      val diffs = JsonDiff.compare(Path.Root, left = expected, right = actual, leftLabel = "expected", rightLabel = "actual")
      MatchResult(
        diffs.isEmpty,
        s"""JSON did not match:${diffs.map(diff => s"\n  - $diff").mkString("")}""",
        """JSON matched""")
    }

  }

}
