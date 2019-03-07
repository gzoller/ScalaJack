package co.blocke.scalajack
package json.primitives.plain

import org.scalatest.{ FunSpec, Matchers }

class Misc() extends FunSpec with Matchers {

  val sj = ScalaJack()

  describe("------------------------\n:  Misc Tests (Plain)  :\n------------------------") {
    it("Read/write null into object") {
      assertResult(null) { sj.read[PlayerMix]("null") }
      assertResult("null") { sj.render[PlayerMix](null) }
    }
  }
}
