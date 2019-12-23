package co.blocke.scalajack
package yaml
package primitives.plain

import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec

import scala.util._

class TryAndCapture() extends AnyFunSpec with Matchers {

  val sj = ScalaJack(YamlFlavor())

  describe(
    "------------------------------------------\n:  Try and Capture Tests (Plain - YAML)  :\n------------------------------------------"
  ) {
    describe("Try:") {
      it("Try sucess") {
        val yaml =
          """name: Greg
            |other:
            |  stuff: [a, b, c]
            |  num: 2""".stripMargin
        val obj = sj.read[Boom](yaml)
        assertResult(0) { yaml.split("\n").toSet.diff(sj.render(obj).split("\n").toSet).size }
        assertResult(true) {
          obj.name == "Greg" && obj.other
            .asInstanceOf[Success[Embed]]
            .get
            .num == 2
        }
      }
      it("Try failure") {
        val yaml =
          """name: Greg
            |other: [1, 2, 3]""".stripMargin
        val obj = sj.read[Boom](yaml)
        assertResult("Line 1: Expected an Object here: +SEQ") {
          obj.other.asInstanceOf[Failure[_]].exception.getMessage
        }
        assertResult("""other:
                       |- 1
                       |- 2
                       |- 3
                       |name: Greg
                       |""".stripMargin) { sj.render(obj) }
      }
      it("Try failure 2") {
        val yaml =
          """name: Greg
            |other: -12.45
            |num: 2""".stripMargin
        val obj = sj.read[Boom](yaml)
        assertResult("Line 1: Expected an Object here: =VAL :-12.45") {
          obj.other.asInstanceOf[Failure[_]].exception.getMessage
        }
        assertResult("""other: -12.45
                       |name: Greg
                       |""".stripMargin) { sj.render(obj) }
      }
    }
    describe("Capture:") {
      it("Plain-class capture can write semantically equivalent JSON") {
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
            |""".stripMargin
        val h = sj.read[Cap](yaml)
        h.name should equal("Greg")
        val yaml2 = sj.render(h)
        yaml.split("\n").toSet.diff(yaml2.split("\n").toSet).size should be(0)
      }
      it("Case class capture can write semantically equivalent JSON") {
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
            |""".stripMargin
        val h = sj.read[CaseCap](yaml)
        h.name should equal("Greg")
        val yaml2 = sj.render(h)
        yaml.split("\n").toSet.diff(yaml2.split("\n").toSet).size should be(0)
      }
      it("Java class capture can write semantically equivalent JSON") {
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
            |""".stripMargin
        val h = sj.read[JavaCap](yaml)
        h.getName should equal("Greg")
        val yaml2 = sj.render(h)
        yaml.split("\n").toSet.diff(yaml2.split("\n").toSet).size should be(0)
      }
    }
  }
}
