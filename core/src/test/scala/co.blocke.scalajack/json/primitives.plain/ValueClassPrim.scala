package co.blocke.scalajack
package json.primitives.plain

import org.scalatest.Matchers
import org.scalatest.funspec.AnyFunSpec

class ValueClassPrim() extends AnyFunSpec with Matchers {

  val sj = ScalaJack()

  describe(
    "----------------------------------------\n:  ValueClass DelimSpec Tests (Plain)  :\n----------------------------------------"
  ) {
      it("Value class of Double") {
        val p1 = new PlayerMix()
        p1.name = "Mike"
        p1.age = VCDouble(BigDecimal("1.23").toDouble)
        val js = sj.render(p1)
        assertResult("""{"age":1.23,"maybe":1,"name":"Mike"}""") { js }
        assertResult(p1.name) {
          val r = sj.read[PlayerMix](js)
          r.name
        }
        assertResult(p1.age) {
          val r = sj.read[PlayerMix](js)
          r.age
        }
      }
    }
}
