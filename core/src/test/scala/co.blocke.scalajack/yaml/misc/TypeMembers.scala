package co.blocke.scalajack
package yaml
package misc

import org.scalatest.funspec.AnyFunSpec
import model._
import util.TypeTags

import scala.reflect.runtime.universe.typeOf

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

class TypeMembers extends AnyFunSpec {

  val sj  = ScalaJack(YamlFlavor())
  val sj2 = ScalaJack(YamlFlavor()).parseOrElse((typeOf[Body] -> typeOf[DefaultBody]))

  describe(
    "-----------------------------\n:  Externalized Type Tests  :\n-----------------------------"
  ) {
    it("Read and match") {
      val yaml =
        """Giraffe: co.blocke.scalajack.yaml.misc.FancyBody
          |id: ABC
          |body:
          |  message: Hello""".stripMargin
      val expected: Envelope[Body] = Envelope("ABC", FancyBody("Hello"))
      assertResult((expected, 1)) {
        val x = sj.read[Envelope[Body]](yaml)
        // Test match functionality
        val num = x.body match {
          case _: FancyBody => 1
          case _            => 2
        }
        (x, num)
      }
    }
    it("Write -- Concrete T value") {
      val value: Envelope[FancyBody] = Envelope("DEF", FancyBody("BOO"))
      val expected =
        """Giraffe: co.blocke.scalajack.yaml.misc.FancyBody
          |id: DEF
          |body:
          |  message: BOO
          |""".stripMargin
      assertResult(expected) {
        sj.render[Envelope[FancyBody]](value)
      }
    }
    it("Write -- Trait T value") {
      val value: Envelope[Body] = Envelope("DEF", FancyBody("BOO"))
      val expected =
        """Giraffe: co.blocke.scalajack.yaml.misc.Body
          |id: DEF
          |body:
          |  _hint: co.blocke.scalajack.yaml.misc.FancyBody
          |  message: BOO
          |""".stripMargin
      assertResult(expected) {
        sj.render[Envelope[Body]](value)
      }
    }
    it("Wrapped") {
      val inst = Bigger(25, Envelope("abc", FancyBody("msg here")))
      val yaml = sj.render(inst)
      assertResult(
        """foo: 25
          |env:
          |  Giraffe: co.blocke.scalajack.yaml.misc.FancyBody
          |  id: abc
          |  body:
          |    message: msg here
          |""".stripMargin
      ) { yaml }
      assertResult(inst) {
        sj.read[Bigger](yaml)
      }
    }
    it("Type modifier works") {
      val sjm = sj.withTypeValueModifier(
        ClassNameHintModifier(
          (hint: String) => "co.blocke.scalajack.yaml.misc." + hint,
          (cname: String) => cname.split('.').last
        )
      )
      val value: Envelope[Body] = Envelope("DEF", FancyBody("BOO"))
      val yaml                  = sjm.render[Envelope[Body]](value)
      assertResult(
        """Giraffe: Body
          |id: DEF
          |body:
          |  _hint: co.blocke.scalajack.yaml.misc.FancyBody
          |  message: BOO
          |""".stripMargin
      ) {
        yaml
      }
      assertResult(value) {
        sjm.read[Envelope[Body]](yaml)
      }
    }
    it("Handles mutliple externalized types (bonus: with modifier)") {
      val sjm = sj.withTypeValueModifier(
        ClassNameHintModifier(
          (hint: String) => "co.blocke.scalajack.yaml.misc." + hint,
          (cname: String) => cname.split('.').last
        )
      )
      val value: BigEnvelope[Body, Hobby, Int] =
        BigEnvelope("DEF", FancyBody("BOO"), InsideHobby("stamps"))
      val yaml = sjm.render[BigEnvelope[Body, Hobby, Int]](value)
      assertResult(Set.empty[String]) {
        """hobby:
          |  _hint: co.blocke.scalajack.yaml.misc.InsideHobby
          |  desc: stamps
          |body:
          |  _hint: co.blocke.scalajack.yaml.misc.FancyBody
          |  message: BOO
          |Hippo: Hobby
          |Giraffe: Body
          |id: DEF
          |""".stripMargin.linesIterator.toSet.diff(yaml.linesIterator.toSet)
      }
      assertResult(value) {
        sjm.read[BigEnvelope[Body, Hobby, Int]](yaml)
      }
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
          |""".stripMargin
      assertResult(value) {
        sjm.read[BigEnvelope[Body, Hobby, Int]](yaml2)
      }
    }
    it("In case we need to use TypeTags vs a type [T] for read") {
      val yaml =
        """Giraffe: co.blocke.scalajack.yaml.misc.FancyBody
          |id: "ABC"
          |body: 
          |  message: Hello""".stripMargin
      val expected: Envelope[Body] = Envelope("ABC", FancyBody("Hello"))
      assertResult(expected) {
        sj.read(yaml)(TypeTags.of(typeOf[Envelope[Body]]))
      }
    }
    it("Works with ParseOrElse") {
      val yaml =
        """Giraffe: co.blocke.scalajack.yaml.misc.FancyBody
          |id: DEF
          |body:
          |  bogus: BOO""".stripMargin
      val expected: Envelope[Body] =
        Envelope("DEF", DefaultBody("Unknown body"))
      assertResult(expected) {
        sj2.read[Envelope[Body]](yaml)
      }
    }
  }
}
