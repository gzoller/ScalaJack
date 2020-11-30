package co.blocke.scalajack
package json.plainclass

import co.blocke.scalajack.model.ClassNameHintModifier
import co.blocke.scala_reflection._
import TestUtil._
import munit._
import munit.internal.console
import co.blocke.scalajack.json.JSON


trait Distance extends Any
case class Meter(val value: Double) extends AnyVal with Distance


class Misc() extends FunSuite:

  val sj = co.blocke.scalajack.ScalaJack()

  test("Read/write null into object") {
    describe("------------------------\n:  Misc Tests (Plain)  :\n------------------------", Console.BLUE)

    assert(null == sj.read[PlayerMix]("null".asInstanceOf[JSON]) )
    assert("null".asInstanceOf[JSON] == sj.render[PlayerMix](null) )
  }

  test("Handles type members with modifier") {
    val prependHintMod = ClassNameHintModifier(
      (hint: String) => "co.blocke.scalajack.json.plainclass." + hint,
      (cname: String) => cname.split('.').last
    )
    val sj2 = co.blocke.scalajack.ScalaJack().withTypeValueModifier(prependHintMod)
    val js = """{"flower":"Flower","rose":{"thing":5,"other":6}}""".asInstanceOf[JSON]
    val inst = sj2.read[WrapTrait[TraitBase]](js)
    assert(inst.rose.isInstanceOf[Flower])
    assertEquals(sj2.render(inst), js)
  }

  test("Fails if no hint for type member") {
    val js = """{"rose":{"thing":5,"other":6}}""".asInstanceOf[JSON]
    val msg =
      """Did not find required type member(s): flower
      |{"rose":{"thing":5,"other":6}}
      |^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[WrapTrait[TraitBase]](js)
    }
  }

  test("Must accept missing default constructor values") {
    val js = """{"foobar":3, "quatro":4, "dontForget":1}""".asInstanceOf[JSON]
    val inst = sj.read[InheritSimpleBase](js)
    assertEquals(inst.one, "blather")
  }

  test("Must accept missing optional constructor values") {
    val js = """{"b":3}""".asInstanceOf[JSON]
    val inst = sj.read[OptConst](js)
    assertEquals(inst.a, None)
    assertEquals(inst.b, Some(3))
  }

  test("Must ignore unneeded type members") {
    val inst = new UnneededType[String]()
    inst.a = 9
    assertEquals(sj.render(inst), """{"a":9}""".asInstanceOf[JSON])
  }

  test("Must require Java classes to have an empty constructor") {
    val inst = new Unsupported("Foo")
    interceptMessage[co.blocke.scalajack.ScalaJackError]("""ScalaJack does not support Java classes with a non-empty constructor."""){
      sj.render(inst)
    }
  }

  test("Must handle Change on Java setter") {
    val js = """{"dos":9}""".asInstanceOf[JSON]
    val inst = sj.read[OnSetter](js)
    assertEquals(inst.getTwo, 9)
    assertEquals(sj.render(inst), js)
  }
