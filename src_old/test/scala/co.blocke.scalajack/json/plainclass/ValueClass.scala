package co.blocke.scalajack
package json
package plainclass

import co.blocke.scalajack.model.ClassNameHintModifier
import co.blocke.scala_reflection._
import TestUtil._
import munit._
import munit.internal.console
import co.blocke.scalajack.json.JSON
import JsonMatcher._


class ValueClass() extends FunSuite:

  val sj = co.blocke.scalajack.ScalaJack()

  test("Value class of Double") {
    describe(
      "----------------------------------------------\n:  ValueClass DelimSpec Tests (Plain Class)  :\n----------------------------------------------", Console.BLUE
    )

    val p1 = new PlayerMix()
    p1.name = "Mike"
    p1.age = VCDouble(BigDecimal("1.23").toDouble)
    val js = sj.render(p1)
    val r = sj.read[PlayerMix](js)
    assert(jsonMatches("""{"age":1.23,"maybe":1,"name":"Mike"}""".asInstanceOf[JSON], js ))
    assertEquals(p1.name, r.name)
    assertEquals(p1.age, r.age)
  }
