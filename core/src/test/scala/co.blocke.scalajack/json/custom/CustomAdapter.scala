package co.blocke.scalajack
package json.custom

import org.scalatest.{ FunSpec, Matchers }

class CustomAdapter() extends FunSpec with Matchers {

  describe("--------------------------\n:  Custom Adapter Tests  :\n--------------------------") {
    it("Overrides type adapter for specific (given) type") {
      val sj = ScalaJack().withAdapters(PhoneAdapter)
      val inst = Person("Bartholomew", "5555555555")
      val js = sj.render(inst)
      assertResult("""{"name":"Bartholomew","phone":"555-555-5555"}""") { js }
      assertResult(inst) {
        sj.read[Person](js)
      }
    }
    it("Overrides type adapter for general type (given type and any parents)") {
      val sj = ScalaJack().withAdapters(OopsPhoneAdapter)
      val inst = Person("Bartholomew", "5555555555")
      val js = sj.render(inst)
      assertResult("""{"name":"Bar-tho-lomew","phone":"555-555-5555"}""") { js }
      assertResult(inst) {
        sj.read[Person](js)
      }
    }
  }
}
