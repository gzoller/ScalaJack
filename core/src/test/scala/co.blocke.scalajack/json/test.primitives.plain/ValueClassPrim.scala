package co.blocke.scalajack
package json.test.primitives.plain

import org.scalatest.{ FunSpec, Matchers }
import java.util.UUID

class ValueClassPrim() extends FunSpec with Matchers {

  val sj = ScalaJack()

  describe("-----------------------------------------\n:  ValueClass Primitives Tests (Plain)  :\n-----------------------------------------") {
    it("Value class of Double") {
      val p1 = new PlayerMix()
      p1.name = "Mike"
      p1.age = VCDouble(BigDecimal("1.23").toDouble)
      val js = sj.render(p1)
      assertResult("""{"age":1.23,"name":"Mike"}""") { js }
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
