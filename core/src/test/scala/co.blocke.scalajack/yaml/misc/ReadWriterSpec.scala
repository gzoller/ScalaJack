package co.blocke.scalajack
package yaml
package misc

import TestUtil._
import munit._
import munit.internal.console
import scala.math.BigInt

trait Human
case class Person(name: String, age: Int) extends Human
case class Typey[T](thing: T) {
  type foom = T
}

class ReadWriterSpec extends FunSuite:

  val sj = ScalaJack(YamlFlavor())

  test("Multiline string | and >") {
    describe(
      "-----------------------------\n:  YamlReader Tests (YAML)  :\n-----------------------------", Console.BLUE
    )
    val yaml =
      """a: |
        |  This
        |  is
        |  a
        |  test
        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(sj.read[Map[String, String]](yaml), Map("a" -> "This\nis\na\ntest"))
    val yaml2 =
      """a: >
        |  This
        |  is
        |  a
        |  test
        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(sj.read[Map[String, String]](yaml2), Map("a" -> "This is a test"))
  }

  test("expectCollection nextText fails") {
    val yaml = """a: foo""".asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 0: Expected a List here: =VAL :foo"){
      sj.read[Map[String, List[Int]]](yaml)
    }
  }

  test("Scan for hint on non-object") {
    interceptMessage[ScalaJackError]("Line 0: Expected an Object here: =VAL :12"){
      sj.read[Person]("12".asInstanceOf[YAML])
    }
  }

  test("Type hint not found in scan") {
    val yaml =
      """name: Fred
        |age: 35
        |""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 2: Type hint '_hint' not found"){
      sj.read[Human](yaml)
    }
  }

  test("Resovle type members on non-object") {
    interceptMessage[ScalaJackError]("Line 0: Expected an Object here: =VAL :12"){
      sj.read[Typey[Person]]("12".asInstanceOf[YAML])
    }
  }

  test("Null Array") {
    describe(
      "-----------------------------\n:  YamlWriter Tests (YAML)  :\n-----------------------------", Console.BLUE
    )
    val yaml =
      """one: [1,2,3]
        |two: null
        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(sj.read[Map[String, List[Int]]](yaml), Map("one" -> List(1, 2, 3), "two" -> null))
  }

  test("Null BigInt") {
    val yaml =
      """one: 123
        |two: null
        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(sj.read[Map[String, BigInt]](yaml), Map("one" -> BigInt(123), "two" -> null))
  }

  test("Empty YamlBuilder") {
    val b = YamlBuilder()
    interceptMessage[ScalaJackError]("No value set for internal yaml builder"){
      b.result()
    }
  }