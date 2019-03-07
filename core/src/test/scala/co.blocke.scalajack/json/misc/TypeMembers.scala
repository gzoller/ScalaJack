package co.blocke.scalajack
package json.misc

import org.scalatest.{ BeforeAndAfterAll, FunSpec, GivenWhenThen }
import model._

import scala.reflect.runtime.universe.typeOf

trait Body
case class FancyBody(message: String) extends Body
case class DefaultBody(message: String = "Unknown body") extends Body
case class AnyBody(stuff: Any) extends Body

trait Hobby
case class InsideHobby(desc: String) extends Hobby

case class Envelope[T <: Body](id: String, body: T) {
  type Giraffe = T
}

// Type member X should be ignored!  Only used internally
case class BigEnvelope[T <: Body, H <: Hobby, X](id: String, body: T, hobby: H) {
  type Giraffe = T
  type Hippo = H
  type IgnoreMe = X

  val x: IgnoreMe = null.asInstanceOf[IgnoreMe]
}

case class Bigger(foo: Int, env: Envelope[FancyBody])

class TypeMembers extends FunSpec with GivenWhenThen with BeforeAndAfterAll {

  val sj = ScalaJack()
  val sj2 = ScalaJack().parseOrElse((typeOf[Body] -> typeOf[DefaultBody]))

  describe("-----------------------------\n:  Externalized Type Tests  :\n-----------------------------") {
    it("Read and match") {
      val json = """{"Giraffe":"co.blocke.scalajack.json.misc.FancyBody","id":"ABC","body":{"message":"Hello"}}"""
      val expected: Envelope[Body] = Envelope("ABC", FancyBody("Hello"))
      assertResult((expected, 1)) {
        val x = sj.read[Envelope[Body]](json)
        // Test match functionality
        val num = x.body match {
          case _: FancyBody => 1
          case _            => 2
        }
        (x, num)
      }
    }
    it("Write") {
      val value: Envelope[Body] = Envelope("DEF", FancyBody("BOO"))
      val expected = """{"Giraffe":"co.blocke.scalajack.json.misc.FancyBody","id":"DEF","body":{"message":"BOO"}}"""
      assertResult(expected) {
        sj.render[Envelope[Body]](value)
      }
    }
    it("Wrapped") {
      val inst = Bigger(25, Envelope("abc", FancyBody("msg here")))
      val js = sj.render(inst)
      assertResult("""{"foo":25,"env":{"Giraffe":"co.blocke.scalajack.json.misc.FancyBody","id":"abc","body":{"message":"msg here"}}}""") { js }
      assertResult(inst) {
        sj.read[Bigger](js)
      }
    }
    it("Type modifier works") {
      val sjm = ScalaJack().withTypeValueModifier(ClassNameHintModifier((hint: String) => "co.blocke.scalajack.json.misc." + hint, (cname: String) => cname.split('.').last))
      val value: Envelope[Body] = Envelope("DEF", FancyBody("BOO"))
      val js = sjm.render[Envelope[Body]](value)
      assertResult("""{"Giraffe":"FancyBody","id":"DEF","body":{"message":"BOO"}}""") {
        js
      }
      assertResult(value) {
        sjm.read[Envelope[Body]](js)
      }
    }
    it("Handles mutliple externalized types (bonus: with modifier)") {
      val sjm = ScalaJack().withTypeValueModifier(ClassNameHintModifier((hint: String) => "co.blocke.scalajack.json.misc." + hint, (cname: String) => cname.split('.').last))
      val value: BigEnvelope[Body, Hobby, Int] = BigEnvelope("DEF", FancyBody("BOO"), InsideHobby("stamps"))
      val js = sjm.render[BigEnvelope[Body, Hobby, Int]](value)
      assertResult("""{"Hippo":"InsideHobby","Giraffe":"FancyBody","id":"DEF","body":{"message":"BOO"},"hobby":{"desc":"stamps"}}""") { js }
      assertResult(value) {
        sjm.read[BigEnvelope[Body, Hobby, Int]](js)
      }
    }
    it("In case we need to use TypeTags vs a type [T] for read") {
      val json = """{"Giraffe":"co.blocke.scalajack.json.misc.FancyBody","id":"ABC","body":{"message":"Hello"}}"""
      val expected: Envelope[Body] = Envelope("ABC", FancyBody("Hello"))
      assertResult(expected) {
        sj.read(json)(TypeTags.of(typeOf[Envelope[Body]]))
      }
    }
    it("Works with ParseOrElse") {
      val js = """{"Giraffe":"co.blocke.scalajack.json.misc.UnknownBody","id":"DEF","body":{"message":"BOO"}}"""
      val expected: Envelope[Body] = Envelope("DEF", DefaultBody("BOO"))
      assertResult(expected) {
        sj2.read[Envelope[Body]](js)
      }
    }
  }
}
