package co.blocke.scalajack
package yaml
package primitives.plain

import TestUtil._
import munit._
import munit.internal.console

class ValueClassPrim() extends FunSuite:

  val sj = ScalaJack(YamlFlavor())

  test("Value class of Double") {
    describe(
      "-----------------------------------------------\n:  ValueClass DelimSpec Tests (Plain - YAML)  :\n-----------------------------------------------", Console.BLUE
    )
    val p1 = new PlayerMix()
    p1.name = "Mike"
    p1.age = VCDouble(BigDecimal("1.23").toDouble)
    val yaml       = sj.render(p1)
    val comparison = """age: 1.23
                        |maybe: 1
                        |name: Mike
                        |""".stripMargin
    assertEquals(yaml.asInstanceOf[String].split("\n").toSet.diff(comparison.split("\n").toSet).size, 0)
    assertEquals(p1.name, {
      val r = sj.read[PlayerMix](yaml)
      r.name
    })
    assertEquals(p1.age, {
      val r = sj.read[PlayerMix](yaml)
      r.age
    })
  }
