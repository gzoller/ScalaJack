package co.blocke.scalajack
package json.custom

import co.blocke.scala_reflection._
import TestUtil._
import munit._
import munit.internal.console
import co.blocke.scalajack.json.JSON

class ParseOrElse() extends FunSuite:

  test("Provide a default object if the object specified in the type hint is unknown") {
    describe("-----------------------\n:  ParseOrElse Tests  :\n-----------------------", Console.BLUE)

    val sj = co.blocke.scalajack.ScalaJack().parseOrElse( (RType.of[Address] -> RType.of[DefaultAddress]) )
    val js =
      """{"_hint":"co.blocke.scalajack.json.custom.USDemographic","age":50,"address":{"_hint":"co.blocke.scalajack.json.custom.UnknownAddress","street":"123 Main","city":"New York","state":"NY","postalCode":"39822"}}""".asInstanceOf[JSON]
    assert(USDemographic(50, DefaultAddress("39822")) == sj.read[Demographic](js))
  }