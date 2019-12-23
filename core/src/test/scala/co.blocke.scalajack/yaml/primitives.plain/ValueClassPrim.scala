package co.blocke.scalajack
package yaml
package primitives.plain

import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec

class ValueClassPrim() extends AnyFunSpec with Matchers {

  val sj = ScalaJack(YamlFlavor())

  describe(
    "-----------------------------------------------\n:  ValueClass DelimSpec Tests (Plain - YAML)  :\n-----------------------------------------------"
  ) {
    it("Value class of Double") {
      val p1 = new PlayerMix()
      p1.name = "Mike"
      p1.age = VCDouble(BigDecimal("1.23").toDouble)
      val yaml       = sj.render(p1)
      val comparison = """age: 1.23
                         |maybe: 1
                         |name: Mike
                         |""".stripMargin
      assertResult(comparison) { yaml }
      assertResult(p1.name) {
        val r = sj.read[PlayerMix](yaml)
        r.name
      }
      assertResult(p1.age) {
        val r = sj.read[PlayerMix](yaml)
        r.age
      }
    }
  }
}
