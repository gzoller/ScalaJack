package co.blocke.scalajack
package json.misc

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ChangeNameSpec extends AnyFunSpec with Matchers {

  val sj = ScalaJack()

  describe(
    "-----------------------\n:  Change Name Tests  :\n-----------------------"
  ) {
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
