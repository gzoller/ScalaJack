package co.blocke.scalajack
package yaml
package primitives.plain

import model.ClassNameHintModifier
import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec

class Misc() extends AnyFunSpec with Matchers {

  val sj = ScalaJack(YamlFlavor())

  describe(
    "-------------------------------\n:  Misc Tests (Plain - YAML)  :\n-------------------------------"
  ) {
    it("Read/write null into object") {
      assertResult(null) { sj.read[PlayerMix]("null") }
      assertResult("""null
                     |""".stripMargin) { sj.render[PlayerMix](null) }
    }
    it("Handles type members with modifier") {
      val prependHintMod = ClassNameHintModifier(
        (hint: String) => "co.blocke.scalajack.yaml.primitives.plain." + hint,
        (cname: String) => cname.split('.').last
      )
      val sj2 = sj.withTypeValueModifier(prependHintMod)
      val yaml =
        """flower: Flower
          |rose:
          |  thing: 5
          |  other: 6
          |""".stripMargin
      val inst = sj2.read[WrapTrait[TraitBase]](yaml)
      inst.rose.isInstanceOf[Flower] should be(true)
      sj2.render(inst) should be(yaml)
    }
    it("Fails if no hint for type member") {
      val yaml =
        """rose:
          |  thing: 5
          |  other: 6""".stripMargin
      the[ScalaJackError] thrownBy sj.read[WrapTrait[TraitBase]](yaml) should have message "Line 0: Did not find required type member(s): flower"
    }
    it("Must accept missing default constructor values") {
      val yaml =
        """foobar: 3
          |quatro: 4
          |dontForget: 1""".stripMargin
      val inst = sj.read[InheritSimpleBase](yaml)
      inst.one should be("blather")
    }
    it("Must accept missing optional constructor values") {
      val yaml = """{}"""
      val inst = sj.read[OptConst](yaml)
      inst.a should be(None)
      inst.b should be(Some(3))
    }
    it("Must ignore unneeded type members") {
      val inst = new UnneededType[String]()
      inst.a = 9
      sj.render(inst) should be("""a: 9
                                  |""".stripMargin)
    }
    it("Must require Java classes to have an empty constructor") {
      val inst = new Unsupported("Foo")
      the[IllegalStateException] thrownBy sj.render(inst) should have message """ScalaJack does not support Java classes with a non-empty constructor."""
    }
    it("Must handle MapName on Java setter") {
      val yaml = "dos: 9"
      val inst = sj.read[OnSetter](yaml)
      inst.getTwo should be(9)
    }
  }
}
