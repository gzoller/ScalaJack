package co.blocke.scalajack
package json.collections

import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec

class Tuples() extends AnyFunSpec with Matchers {

  val sj = ScalaJack()

  describe("-----------------\n:  Tuple Tests  :\n-----------------") {
    it("null tuples work") {
      val jsNull = "null"
      assertResult(null) { sj.read[(Int, Boolean)](jsNull) }
      sj.read[(Int, Boolean)]("null") should be(null)
    }
    it("missing start bracken") {
      val js = """12,5"""
      val msg =
        """Expected start of tuple here
          |12,5
          |^""".stripMargin
      the[ScalaJackError] thrownBy sj.read[(Int, Int)](js) should have message msg
    }
    it("missing comma") {
      val js = """[12"""
      val msg =
        """Expected comma here
        |[12
        |---^""".stripMargin
      the[ScalaJackError] thrownBy sj.read[(Int, Int)](js) should have message msg
    }
    it("no closing bracket") {
      val js = """[12,5"""
      val msg =
        """Expected end of tuple here
          |[12,5
          |-----^""".stripMargin
      the[ScalaJackError] thrownBy sj.read[(Int, Int)](js) should have message msg
    }
  }
}
