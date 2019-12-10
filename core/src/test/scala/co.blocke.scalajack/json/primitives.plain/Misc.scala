package co.blocke.scalajack
package json.primitives.plain

import model.ClassNameHintModifier
import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec

class Misc() extends AnyFunSpec with Matchers {

  val sj = ScalaJack()

  describe(
    "------------------------\n:  Misc Tests (Plain)  :\n------------------------"
  ) {
      it("Read/write null into object") {
        assertResult(null) { sj.read[PlayerMix]("null") }
        assertResult("null") { sj.render[PlayerMix](null) }
      }
      it("Handles type members with modifier") {
        val prependHintMod = ClassNameHintModifier(
          (hint: String) => "co.blocke.scalajack.json.primitives.plain." + hint,
          (cname: String) => cname.split('.').last
        )
        val sj2 = ScalaJack().withTypeValueModifier(prependHintMod)
        val js = """{"flower":"Flower","rose":{"thing":5,"other":6}}"""
        val inst = sj2.read[WrapTrait[TraitBase]](js)
        inst.rose.isInstanceOf[Flower] should be(true)
        sj2.render(inst) should be(js)
      }
      it("Fails if no hint for type member") {
        val js = """{"rose":{"thing":5,"other":6}}"""
        val msg =
          """Did not find required type member(s): flower
          |{"rose":{"thing":5,"other":6}}
          |^""".stripMargin
        the[ScalaJackError] thrownBy sj.read[WrapTrait[TraitBase]](js) should have message msg
      }
      it("Must accept missing default constructor values") {
        val js = """{"foobar":3, "quatro":4, "dontForget":1}"""
        val inst = sj.read[InheritSimpleBase](js)
        inst.one should be("blather")
      }
      it("Must accept missing optional constructor values") {
        val js = """{}"""
        val inst = sj.read[OptConst](js)
        inst.a should be(None)
        inst.b should be(Some(3))
      }
      it("Must ignore unneeded type members") {
        val inst = new UnneededType[String]()
        inst.a = 9
        sj.render(inst) should be("""{"a":9}""")
      }
      it("Must require Java classes to have an empty constructor") {
        val inst = new Unsupported("Foo")
        the[IllegalStateException] thrownBy sj.render(inst) should have message """ScalaJack does not support Java classes with a non-empty constructor."""
      }
      it("Must handle MapName on Java setter") {
        val js = """{"dos":9}"""
        val inst = sj.read[OnSetter](js)
        inst.getTwo should be(9)
      }
    }
}
