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
      p1.age = VCDouble(1.23)
      val js = sj.render(p1)
      println(js)
      assertResult("""{"age":1.23,"name":"Mike"}""") { js }
      assertResult(true) {
        val r = sj.read[PlayerMix](js)
        (r.name == p1.name && r.age == p1.age)
      }
    }
  }
}
