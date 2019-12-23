package co.blocke.scalajack
package yaml
package misc

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

object MyTypes {
  type Phone = String
}
import MyTypes._
import model._

import scala.collection.mutable

case class PersonWithPhone(name: String, phone: Phone)

object PhoneAdapter extends TypeAdapter.===[Phone] with Stringish {
  def read(parser: Parser): Phone =
    parser.expectString() match {
      case null      => null
      case s: String => s.replaceAll("-", "")
    }

  def write[WIRE](t: Phone, writer: Writer[WIRE], out: mutable.Builder[WIRE, WIRE]): Unit = t match {
    case null => writer.writeNull(out)
    case _ =>
      writer.writeString(
        "%s-%s-%s".format(t.substring(0, 3), t.substring(3, 6), t.substring(6)),
        out
      )
  }
}

class YamlFlavorSpec extends AnyFunSpec with Matchers {

  val sj = ScalaJack(YamlFlavor())

  describe(
    "-----------------------------\n:  YamlFlavor Tests (YAML)  :\n-----------------------------"
  ) {
    it("forType.read") {
      val forType = sj.forType[Person]
      forType.read("""name: Fred
                     |age: 35
                     |""".stripMargin) should be(Person("Fred", 35))
    }
    it("forType.read[T]") {
      val forType = sj.forType[Person]
      forType.read[PersonWithPhone]("""name: Fred
                                      |phone: 5555555555
                                      |""".stripMargin) should be(PersonWithPhone("Fred", "5555555555"))
    }
    it("forType.render") {
      val forType = sj.forType[Person]
      forType.render(Person("Fred", 35)) should be("""name: Fred
                                                     |age: 35
                                                     |""".stripMargin)
    }
    it("forType.render[T]") {
      val forType = sj.forType[Person]
      forType.render(PersonWithPhone("Fred", "5555555555")) should be("""name: Fred
                                                                        |phone: '5555555555'
                                                                        |""".stripMargin)
    }
    it("forType.forType") {
      val forType = sj.forType[Person].forType[PersonWithPhone]
      forType.render(PersonWithPhone("Fred", "5555555555")) should be("""name: Fred
                                                                        |phone: '5555555555'
                                                                        |""".stripMargin)
    }
    it("parse") {
      val parser = sj.parse("Foo")
      parser.expectString() should be("Foo")
    }
    it("allowPermissivePrimitives") {
      the[ScalaJackError] thrownBy (sj.allowPermissivePrimitives) should have message "Not available for YAML encoding"
    }
    it("withAdapters: Overrides type adapter for specific (given) type") {
      val sj2  = sj.withAdapters(PhoneAdapter)
      val inst = PersonWithPhone("Bartholomew", "5555555555")
      val yaml = sj2.render(inst)
      assertResult("""name: Bartholomew
                     |phone: 555-555-5555
                     |""".stripMargin) { yaml }
      assertResult(inst) {
        sj2.read[PersonWithPhone](yaml)
      }
    }
    it("withDefaultHint") {
      val sj2 = sj.withDefaultHint("foom")
      val yaml =
        """foom: co.blocke.scalajack.yaml.misc.Person
          |name: Fred
          |age: 34
          |""".stripMargin
      sj2.render[Human](Person("Fred", 34)) should be(yaml)
    }
  }
}
