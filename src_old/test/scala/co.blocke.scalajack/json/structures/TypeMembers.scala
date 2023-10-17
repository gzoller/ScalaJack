package co.blocke.scalajack
package json.structures

import co.blocke.scala_reflection._
import TestUtil._
import munit._
import munit.internal.console
import co.blocke.scalajack.json.JSON

trait Thing
case class BigThing(s: String) extends Thing
case class Boo[T <: Thing]( a: T )


class TypeMembers extends FunSuite:

  val sj = co.blocke.scalajack.ScalaJack()
  val sj2 = co.blocke.scalajack.ScalaJack().parseOrElse((RType.of[Body] -> RType.of[DefaultBody]))

  test("Read and match") {
    describe("-----------------------------\n:  Externalized Type Tests  :\n-----------------------------",Console.BLUE)

    val json =
      """{"Giraffe":"co.blocke.scalajack.json.structures.FancyBody","id":"ABC","body":{"message":"Hello"}}""".asInstanceOf[JSON]
    val expected: Envelope[Body] = Envelope("ABC", FancyBody("Hello"))
    val x = sj.read[Envelope[Body]](json)
    // Test match functionality
    val num = x.body match {
      case _: FancyBody => 1
      case _            => 2
    }
    assertEquals((expected, 1),(x, num))
  }

  test("Write -- Concrete T value") {
    val value: Envelope[FancyBody] = Envelope("DEF", FancyBody("BOO"))
    val expected =
      """{"Giraffe":"co.blocke.scalajack.json.structures.FancyBody","id":"DEF","body":{"message":"BOO"}}""".asInstanceOf[JSON]
    assertEquals(expected, sj.render[Envelope[FancyBody]](value))
  }

  test("Write -- Trait T value") {
    val value: Envelope[Body] = Envelope("DEF", FancyBody("BOO"))
    val expected =
      """{"Giraffe":"co.blocke.scalajack.json.structures.FancyBody","id":"DEF","body":{"message":"BOO"}}""".asInstanceOf[JSON]
    assertEquals(expected, sj.render[Envelope[Body]](value))
  }

  test("Wrapped") {
    val inst = Bigger(25, Envelope("abc", FancyBody("msg here")))
    val js = sj.render(inst)
    assertEquals(
      """{"foo":25,"env":{"Giraffe":"co.blocke.scalajack.json.structures.FancyBody","id":"abc","body":{"message":"msg here"}}}""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[Bigger](js))
  }

  test("Type modifier works") {
    val sjm = co.blocke.scalajack.ScalaJack().withTypeValueModifier(
      co.blocke.scalajack.model.ClassNameHintModifier(
        (hint: String) => "co.blocke.scalajack.json.structures." + hint,
        (cname: String) => cname.split('.').last
      )
    )
    val value: Envelope[Body] = Envelope("DEF", FancyBody("BOO"))
    val js = sjm.render[Envelope[Body]](value)
    assertEquals(
      """{"Giraffe":"FancyBody","id":"DEF","body":{"message":"BOO"}}""".asInstanceOf[JSON], js)
    assertEquals(value, sjm.read[Envelope[Body]](js))
  }

  test("Handles mutliple externalized types (bonus: with modifier)") {
    val sjm = co.blocke.scalajack.ScalaJack().withTypeValueModifier(
      co.blocke.scalajack.model.ClassNameHintModifier(
        (hint: String) => "co.blocke.scalajack.json.structures." + hint,
        (cname: String) => cname.split('.').last
      )
    )
    val value: BigEnvelope[Body, Hobby, Int] =
      BigEnvelope("DEF", FancyBody("BOO"), InsideHobby("stamps"))
    val js = sjm.render[BigEnvelope[Body, Hobby, Int]](value)
    assertEquals(
      """{"Giraffe":"FancyBody","Hippo":"InsideHobby","id":"DEF","body":{"message":"BOO"},"hobby":{"desc":"stamps"}}""".asInstanceOf[JSON],
      js)
    assertEquals(value, sjm.read[BigEnvelope[Body, Hobby, Int]](js))
  }
  
  test("Works with ParseOrElse") {
    val js =
      """{"Giraffe":"co.blocke.scalajack.json.structures.FancyBody","id":"DEF","body":{"bogus":"BOO"}}""".asInstanceOf[JSON]
    val expected: Envelope[Body] = Envelope("DEF", DefaultBody("Unknown body"))
    assertEquals(expected,sj2.read[Envelope[Body]](js))
  }
