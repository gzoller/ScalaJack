package co.blocke.scalajack
package json.test.misc

import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
import org.scalatest.Matchers._

class MapNameSpec extends FunSpec with GivenWhenThen with BeforeAndAfterAll {

  val sj = ScalaJack()

  describe("------------------\n:  MapName Tests  :\n------------------") {
    it("Mapping name for case class fields works") {
      val js = """{"foo_bar":"thing","a_b":123,"count":2}"""
      val r = sj.read[MapFactor](js)
      r should be(MapFactor("thing", 123L, 2))
      sj.render(r) should be(js)
    }
    it("Mapping name for non-case class fields works") {
      val js = """{"count":2,"a_b":123,"foo_bar":"thing"}"""
      val mfp = new MapFactorPlain()
      mfp.fooBar = "thing"
      mfp.thingy = 123L
      mfp.count = 2
      sj.render(mfp) should be(js)
      assertResult(true) {
        val r = sj.read[MapFactorPlain](js)
        (r.fooBar == mfp.fooBar && r.thingy == mfp.thingy && r.count == mfp.count)
      }
    }
  }
}
