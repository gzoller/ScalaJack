package co.blocke.scalajack
package test
package custom

import org.scalatest.{ FunSpec, Matchers }
import scala.reflect.runtime.universe.typeOf

class ParseOrElse() extends FunSpec with Matchers {

  describe("-----------------------\n:  ParseOrElse Tests  :\n-----------------------") {
    it("Provide a default object if the object specified in the type hint is unknown") {
      val sj = ScalaJack().parseOrElse((typeOf[Address] -> typeOf[DefaultAddress]))
      val js = """{"_hint":"co.blocke.scalajack.test.custom.USDemographic","age":50,"address":{"_hint":"co.blocke.scalajack.test.custom.UnknownAddress","street":"123 Main","city":"New York","state":"NY","postalCode":"39822"}}"""
      assertResult(USDemographic(50, DefaultAddress("39822"))) {
        sj.read[USDemographic](js)
      }
    }
  }
}