package co.blocke.scalajack
package json.test.primitives.plain

import org.scalatest.{ FunSpec, Matchers }

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
      val msg = """DeserializationException(1 error):
  [$.age] Required field missing (reported by: DerivedValueClassDeserializer[co.blocke.scalajack.json.test.primitives.plain.VCDouble, scala.Double])""".stripMargin

      the[co.blocke.scalajack.DeserializationException] thrownBy sj.read[PlayerMix](js) should have message msg
    }
    it("Read/write null into object") {
      assertResult(null) { sj.read[PlayerMix]("null") }
      assertResult("null") { sj.render[PlayerMix](null) }
    }
  }
}
