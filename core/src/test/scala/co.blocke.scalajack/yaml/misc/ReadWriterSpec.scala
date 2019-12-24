package co.blocke.scalajack
package yaml
package misc

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import scala.math.BigInt

trait Human
case class Person(name: String, age: Int) extends Human
case class Typey[T](thing: T) {
  type foom = T
}

class ReadWriterSpec extends AnyFunSpec with Matchers {

  val sj = ScalaJack(YamlFlavor())

  describe(
    "-----------------------------\n:  YamlReader Tests (YAML)  :\n-----------------------------"
  ) {
    it("Multiline string | and >") {
      val yaml =
        """a: |
          |  This
          |  is
          |  a
          |  test
          |""".stripMargin
      sj.read[Map[String, String]](yaml) should be(Map("a" -> "This\nis\na\ntest"))
      val yaml2 =
        """a: >
          |  This
          |  is
          |  a
          |  test
          |""".stripMargin
      sj.read[Map[String, String]](yaml2) should be(Map("a" -> "This is a test"))
    }
    it("expectCollection nextText fails") {
      val yaml = """a: foo"""
      the[ScalaJackError] thrownBy sj.read[Map[String, List[Int]]](yaml) should have message "Line 0: Expected a List here: =VAL :foo"
    }
    it("Scan for hint on non-object") {
      the[ScalaJackError] thrownBy sj.read[Person]("12") should have message "Line 0: Expected an Object here: =VAL :12"
    }
    it("Type hint not found in scan") {
      val yaml =
        """name: Fred
          |age: 35
          |""".stripMargin
      the[ScalaJackError] thrownBy sj.read[Human](yaml) should have message "Line 2: Type hint '_hint' not found"
    }
    it("Resovle type members on non-object") {
      the[ScalaJackError] thrownBy sj.read[Typey[Person]]("12") should have message "Line 0: Expected an Object here: =VAL :12"
    }
  }
  describe(
    "-----------------------------\n:  YamlWriter Tests (YAML)  :\n-----------------------------"
  ) {
    it("Null Array") {
      val yaml =
        """one: [1,2,3]
          |two: null
          |""".stripMargin
      sj.read[Map[String, List[Int]]](yaml) should be(Map("one" -> List(1, 2, 3), "two" -> null))
    }
    it("Null BigInt") {
      val yaml =
        """one: 123
          |two: null
          |""".stripMargin
      sj.read[Map[String, BigInt]](yaml) should be(Map("one" -> BigInt(123), "two" -> null))
    }
    it("Empty YamlBuilder") {
      val b = YamlBuilder()
      the[ScalaJackError] thrownBy b.result() should have message "No value set for internal yaml builder"
    }
  }
}
