package co.blocke.scalajack
package json.custom

import co.blocke.scala_reflection._
import TestUtil._
import munit._
import munit.internal.console
import co.blocke.scalajack.json.JSON

class CustomAdapter() extends FunSuite:

  test("Overrides type adapter for specific (given) type") {
    describe("--------------------------\n:  Custom Adapter Tests  :\n--------------------------", Console.BLUE)

    val sj = co.blocke.scalajack.ScalaJack().withAdapters(PhoneAdapter)
    val inst = Person("Bartholomew", "5555555555".asInstanceOf[Phone])
    val js = sj.render(inst)
    assertEquals("""{"name":"Bartholomew","phone":"555-555-5555"}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[Person](js))
  }