package co.blocke.scalajack
package json

import org.json4s.JsonAST.JValue
import org.json4s.native.JsonMethods._

import munit.internal.MacroCompat
import munit.{Location, Compare}
import munit.internal.console.StackTraces
import munit.internal.difflib.ComparisonFailExceptionHandler

// object JsonMatcher {

//   def jsonMatches( expected: String, actual: String ): Boolean = 
//     val diffs = JsonDiff.compare(
//       parseJValue(expected),
//       parseJValue(actual),
//       "expected",
//       "actual"
//     )
//     diffs.isEmpty

//   implicit def parseJValue(string: String): JValue = parse(string)
// }


object BlockeUtil extends BlockeUtil
trait BlockeUtil extends MacroCompat.CompileErrorMacro with munit.Assertions:

  // val munitLines = new Lines

  // def munitAnsiColors: Boolean = true

  implicit def parseJValue(string: String): JValue = parse(string)

  /**
   * Asserts that two JSON strings are equal according to the `Compare[A, B]` type-class.
   *
   * By default, uses `==` to compare values.
   * 
   * JSON is unorderd, so two JSON strings are equal if they contain the same elements, in any order
   */
  def jsonMatches(
      obtained: String,
      expected: String,
      clue: => Any = "values are not the same"
  )(implicit loc: Location, compare: Compare[String, String]): Unit = {
    StackTraces.dropInside {
      val areEqual = 
        val diffs = JsonDiff.compare(
          parseJValue(obtained),
          parseJValue(expected),
          "obtained",
          "expected"
        )
        diffs.isEmpty

      if (!areEqual) 
        compare.failEqualsComparison(obtained, expected, clue, loc, this)
    }
  }