package co.blocke.scalajack
package yaml
package primitives.plain

import TestUtil._
import munit._
import munit.internal.console

import scala.util._

class TryAndCapture() extends FunSuite:

  val sj = ScalaJack(YamlFlavor())

  test("Try sucess") {
    describe(
      "------------------------------------------\n:  Try and Capture Tests (Plain - YAML)  :\n------------------------------------------", Console.BLUE
    )
    describe("Try:")
    val yaml =
      """name: Greg
        |other:
        |  stuff: [a, b, c]
        |  num: 2""".stripMargin.asInstanceOf[YAML]
    val obj = sj.read[Boom](yaml)
    assertEquals(0,  yaml.asInstanceOf[String].split("\n").toSet.diff(sj.render(obj).asInstanceOf[String].split("\n").toSet).size )
    assertEquals(true, 
      obj.name == "Greg" && obj.other
        .asInstanceOf[Success[Embed]]
        .get
        .num == 2
    )
  }

  test("Try failure") {
    val yaml =
      """name: Greg
        |other: [1, 2, 3]""".stripMargin.asInstanceOf[YAML]
    val obj = sj.read[Boom](yaml)
    assertEquals("Line 1: Expected an Object here: +SEQ",  obj.other.asInstanceOf[Failure[_]].exception.getMessage )
    assertEquals("""other:
      |- 1
      |- 2
      |- 3
      |name: Greg
      |""".stripMargin.split("\n").toSet.diff(sj.render(obj).asInstanceOf[String].split("\n").toSet).size, 0)
  }

  test("Try failure 2") {
    val yaml =
      """name: Greg
        |other: -12.45
        |num: 2""".stripMargin.asInstanceOf[YAML]
    val obj = sj.read[Boom](yaml)
    assertEquals("Line 1: Expected an Object here: =VAL :-12.45",  obj.other.asInstanceOf[Failure[_]].exception.getMessage )
    assertEquals("""other: -12.45
      |name: Greg
      |""".stripMargin.split("\n").toSet.diff(sj.render(obj).asInstanceOf[String].split("\n").toSet).size, 0)
  }

  test("Plain-class capture can write semantically equivalent JSON") {
    describe("Capture:")
    val yaml =
      """name: Greg
        |foo:
        |- 1
        |- 2
        |- t
        |zing:
        |  dot:
        |    age: 25
        |    food: Pizza
        |blather: wow
        |boo: -29384.34
        |maybe: false
        |""".stripMargin.asInstanceOf[YAML]
    val h = sj.read[Cap](yaml)
    assertEquals(h.name, "Greg")
    val yaml2 = sj.render(h)
    assertEquals(yaml.asInstanceOf[String].split("\n").toSet.diff(yaml2.asInstanceOf[String].split("\n").toSet).size, 0)
  }

  test("Case class capture can write semantically equivalent JSON") {
    val yaml =
      """name: Greg
        |foo: [1, 2, t]
        |zing:
        |  _hint: a.b.com.Hey
        |  dot:
        |    age: 25
        |    food: Pizza
        |  blather: wow
        |  boo: -29384.34
        |  maybe: false
        |""".stripMargin.asInstanceOf[YAML]
    val h = sj.read[CaseCap](yaml)
    assertEquals(h.name, "Greg")
    val yaml2 = sj.render(h)
    assertEquals(yaml.asInstanceOf[String].split("\n").toSet.diff(yaml2.asInstanceOf[String].split("\n").toSet).size, 0)
  }

  test("Java class capture can write semantically equivalent JSON") {
    val yaml =
      """name: Greg
        |foo: [1, 2, t]
        |zing:
        |  _hint: a.b.com.Hey
        |  dot:
        |    age: 25
        |    food: Pizza
        |  blather: wow
        |  boo: -29384.34
        |  maybe: false
        |""".stripMargin.asInstanceOf[YAML]
    val h = sj.read[JavaCap](yaml)
    assertEquals(h.getName, "Greg")
    val yaml2 = sj.render(h)
    assertEquals(yaml.asInstanceOf[String].split("\n").toSet.diff(yaml2.asInstanceOf[String].split("\n").toSet).size, 0)
  }