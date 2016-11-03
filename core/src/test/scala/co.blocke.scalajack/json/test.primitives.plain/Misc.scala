package co.blocke.scalajack
package json.test.primitives.plain

import org.scalatest.{ FunSpec, Matchers }
import java.util.UUID

class Misc() extends FunSpec with Matchers {

  val sj = ScalaJack()

  describe("------------------------\n:  Misc Tests (Plain)  :\n------------------------") {
    it("Not all constructor fields marked with val") {
      val inst = new NotAllVals(1, 2, 3)
      val msg = "Unable to find a type adapter for co.blocke.scalajack.json.test.primitives.plain.NotAllVals"
      the[java.lang.IllegalArgumentException] thrownBy sj.render(inst) should have message msg
    }
    it("Missing field (non-optional) from a getter/setter class") {
      val js = """{"name":"Mike"}"""
      val msg = """Required field age in class co.blocke.scalajack.json.test.primitives.plain.PlayerMix is missing from input and has no specified default value
       |{"name":"Mike"}
       |--------------^""".stripMargin
      the[java.lang.IllegalStateException] thrownBy sj.read[PlayerMix](js) should have message msg
    }
    it("Read/write null into object") {
      assertResult(null) { sj.read[PlayerMix]("null") }
      assertResult("null") { sj.render[PlayerMix](null) }
    }
  }
}
