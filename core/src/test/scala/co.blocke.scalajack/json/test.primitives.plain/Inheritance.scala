package co.blocke.scalajack
package json.test.primitives.plain

import org.scalatest.{ FunSpec, Matchers }
import java.util.UUID

class Inheritance() extends FunSpec with Matchers {

  val sj = ScalaJack()

  describe("-------------------------------\n:  Inheritance Tests (Plain)  :\n-------------------------------") {
    it("Class inheritance must work") {
      val p1 = new BigPlayer()
      p1.name = "Mike"
      p1.age = VCDouble(1.23)
      p1.more = 25
      val js = sj.render(p1)
      assertResult("""{"more":25,"age":1.23,"name":"Mike"}""") { js }
      assertResult(true) {
        val r = sj.read[BigPlayer](js)
        (r.name == p1.name && r.age == p1.age && r.more == p1.more)
      }
    }
  }
}
