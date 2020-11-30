package co.blocke.scalajack
package yaml
package primitives.plain

import model.ClassNameHintModifier
import TestUtil._
import munit._
import munit.internal.console

class Misc() extends FunSuite:

  val sj = ScalaJack(YamlFlavor())

  test("Read/write null into object") {
    describe(
      "-------------------------------\n:  Misc Tests (Plain - YAML)  :\n-------------------------------", Console.BLUE
    )
    assert(null == sj.read[PlayerMix]("null".asInstanceOf[YAML]) )
    assertEquals("""null
                    |""".stripMargin, sj.render[PlayerMix](null).asInstanceOf[String] )
  }

  test("Handles type members with modifier") {
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
        |""".stripMargin.asInstanceOf[YAML]
    val inst = sj2.read[WrapTrait[TraitBase]](yaml)
    assertEquals(inst.rose.isInstanceOf[Flower], true)
    assertEquals(sj2.render(inst), yaml)
  }

  test("Fails if no hint for type member") {
    val yaml =
      """rose:
        |  thing: 5
        |  other: 6""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 0: Did not find required type member(s): flower"){
      sj.read[WrapTrait[TraitBase]](yaml)
    }
  }

  test("Must accept missing default constructor values") {
    val yaml =
      """foobar: 3
        |quatro: 4
        |dontForget: 1""".stripMargin.asInstanceOf[YAML]
    val inst = sj.read[InheritSimpleBase](yaml)
    assertEquals(inst.one, "blather")
  }

  test("Must accept missing optional constructor values") {
    val yaml = """{}""".asInstanceOf[YAML]
    val inst = sj.read[OptConst](yaml)
    assertEquals(inst.a, None)
    assertEquals(inst.b, Some(3))
  }

  test("Must ignore unneeded type members") {
    val inst = new UnneededType[String]()
    inst.a = 9
    assertEquals(sj.render(inst).asInstanceOf[String], """a: 9
                                |""".stripMargin)
  }

  test("Must require Java classes to have an empty constructor") {
    val inst = new Unsupported("Foo")
    interceptMessage[ScalaJackError]("""ScalaJack does not support Java classes with a non-empty constructor."""){
      sj.render(inst)
    }
  }

  test("Must handle MapName on Java setter") {
    val yaml = "dos: 9".asInstanceOf[YAML]
    val inst = sj.read[OnSetter](yaml)
    assertEquals(inst.getTwo, 9)
  }