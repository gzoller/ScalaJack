package co.blocke.scalajack
package json.custom

import org.scalatest.Matchers
import org.scalatest.funspec.AnyFunSpec
import scala.reflect.runtime.universe.typeOf

class ParseOrElse() extends AnyFunSpec with Matchers {

  describe(
    "-----------------------\n:  ParseOrElse Tests  :\n-----------------------"
  ) {
      it(
        "Provide a default object if the object specified in the type hint is unknown"
      ) {
          val sj =
            ScalaJack().parseOrElse((typeOf[Address] -> typeOf[DefaultAddress]))
          val js =
            """{"_hint":"co.blocke.scalajack.json.custom.USDemographic","age":50,"address":{"_hint":"co.blocke.scalajack.json.custom.UnknownAddress","street":"123 Main","city":"New York","state":"NY","postalCode":"39822"}}"""
          assertResult(USDemographic(50, DefaultAddress("39822"))) {
            sj.read[Demographic](js)
          }
        }
    }
}
