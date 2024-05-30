package co.blocke.scalajack
package json

import org.json4s.JsonAST.JValue
import org.json4s.native.JsonMethods.*

import org.scalatest.*
import matchers.*

trait JsonMatchers {
  class JsonMatchesMatcher(expectedJson: String) extends Matcher[String]:
    def apply(srcJson: String) =
      val diffs = JsonDiff.compare(
        parseJValue(srcJson),
        parseJValue(expectedJson),
        "expected",
        "actual"
      )
      MatchResult(
        diffs.isEmpty,
        s"""JSON values did not match""",
        s"""JSON values matched"""
      )

  def matchJson(targetJson: String) = new JsonMatchesMatcher(targetJson)
  implicit def parseJValue(string: String): JValue = parse(string)
}
object JsonMatchers extends JsonMatchers
