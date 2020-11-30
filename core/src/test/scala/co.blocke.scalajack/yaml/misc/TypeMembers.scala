package co.blocke.scalajack
package yaml
package misc

import TestUtil._
import munit._
import munit.internal.console
import co.blocke.scala_reflection.RType
import model._


trait Body
case class FancyBody(message: String)                    extends Body
case class DefaultBody(message: String = "Unknown body") extends Body
case class AnyBody(stuff: Any)                           extends Body

trait Hobby
case class InsideHobby(desc: String) extends Hobby

case class Envelope[T <: Body](id: String, body: T) {
  type Giraffe = T
}

// Type member X should be ignored!  Only used internally
case class BigEnvelope[T <: Body, H <: Hobby, X](id: String, body: T, hobby: H) {
  type Giraffe  = T
  type Hippo    = H
  type IgnoreMe = X

  val x: IgnoreMe = null.asInstanceOf[IgnoreMe]
}

case class Bigger(foo: Int, env: Envelope[FancyBody])

class TypeMembers extends FunSuite:

  val sj  = ScalaJack(YamlFlavor())
  val sj2 = ScalaJack(YamlFlavor()).parseOrElse((RType.of[Body] -> RType.of[DefaultBody]))

  test("Read and match") {
    describe(
      "------------------------------------\n:  Externalized Type Tests (YAML)  :\n------------------------------------", Console.BLUE
    )
    val yaml =
      """Giraffe: co.blocke.scalajack.yaml.misc.FancyBody
        |id: ABC
        |body:
        |  message: Hello""".stripMargin.asInstanceOf[YAML]
    val expected: Envelope[Body] = Envelope("ABC", FancyBody("Hello"))
    assertEquals((expected, 1), {
      val x = sj.read[Envelope[Body]](yaml)
      // Test match functionality
      val num = x.body match {
        case _: FancyBody => 1
        case _            => 2
      }
      (x, num)
    })
  }

  test("Write -- Concrete T value") {
    val value: Envelope[FancyBody] = Envelope("DEF", FancyBody("BOO"))
    val expected =
      """Giraffe: co.blocke.scalajack.yaml.misc.FancyBody
        |id: DEF
        |body:
        |  message: BOO
        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(expected, sj.render[Envelope[FancyBody]](value))
  }

  test("Write -- Trait T value") {
    val value: Envelope[Body] = Envelope("DEF", FancyBody("BOO"))
    val expected =
      """Giraffe: co.blocke.scalajack.yaml.misc.FancyBody
        |id: DEF
        |body:
        |  message: BOO
        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(expected, sj.render[Envelope[Body]](value))
  }

  test("Wrapped") {
    val inst = Bigger(25, Envelope("abc", FancyBody("msg here")))
    val yaml = sj.render(inst)
    assertEquals(
      """foo: 25
        |env:
        |  Giraffe: co.blocke.scalajack.yaml.misc.FancyBody
        |  id: abc
        |  body:
        |    message: msg here
        |""".stripMargin.asInstanceOf[YAML], yaml)
    assertEquals(inst, sj.read[Bigger](yaml))
  }

  test("Type modifier works") {
    val sjm = sj.withTypeValueModifier(
      ClassNameHintModifier(
        (hint: String) => "co.blocke.scalajack.yaml.misc." + hint,
        (cname: String) => cname.split('.').last
      )
    )
    val value: Envelope[Body] = Envelope("DEF", FancyBody("BOO"))
    val yaml                  = sjm.render[Envelope[Body]](value)
    assertEquals(
      """Giraffe: FancyBody
        |id: DEF
        |body:
        |  message: BOO
        |""".stripMargin.asInstanceOf[YAML], yaml)
    assertEquals(value, sjm.read[Envelope[Body]](yaml))
  }

  test("Handles mutliple externalized types (bonus: with modifier)") {
    val sjm = sj.withTypeValueModifier(
      ClassNameHintModifier(
        (hint: String) => "co.blocke.scalajack.yaml.misc." + hint,
        (cname: String) => cname.split('.').last
      )
    )
    val value: BigEnvelope[Body, Hobby, Int] =
      BigEnvelope("DEF", FancyBody("BOO"), InsideHobby("stamps"))
    val yaml = sjm.render[BigEnvelope[Body, Hobby, Int]](value)
    assertEquals(Set.empty[String],
      """hobby:
        |  desc: stamps
        |body:
        |  message: BOO
        |Hippo: InsideHobby
        |Giraffe: FancyBody
        |id: DEF
        |""".stripMargin.linesIterator.toSet.diff(yaml.asInstanceOf[String].linesIterator.toSet)
    )
    assertEquals(value, sjm.read[BigEnvelope[Body, Hobby, Int]](yaml))
    // Test out-of-order type hint--test skipping element logic in parser
    val yaml2 =
      """hobby:
        |  _hint: co.blocke.scalajack.yaml.misc.InsideHobby
        |  desc: stamps
        |body:
        |  extra: 
        |     - a
        |     - b
        |     - c: foom
        |  _hint: co.blocke.scalajack.yaml.misc.FancyBody
        |  message: BOO
        |Hippo: Hobby
        |Giraffe: Body
        |id: DEF
        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(value, sjm.read[BigEnvelope[Body, Hobby, Int]](yaml2))
  }

  test("Works with ParseOrElse") {
    val yaml =
      """Giraffe: co.blocke.scalajack.yaml.misc.FancyBody
        |id: DEF
        |body:
        |  bogus: BOO""".stripMargin.asInstanceOf[YAML]
    val expected: Envelope[Body] =
      Envelope("DEF", DefaultBody("Unknown body"))
    assertEquals(expected, sj2.read[Envelope[Body]](yaml))
  }