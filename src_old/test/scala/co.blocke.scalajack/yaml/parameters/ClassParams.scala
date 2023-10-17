package co.blocke.scalajack
package yaml
package parameters

import TestUtil._
import munit._
import munit.internal.console

class ClassParams extends FunSuite:

  val sj = ScalaJack(YamlFlavor())

  test("Simple parameters - Foo[A](x:A) where A -> simple type") {
    describe(
      "----------------------------------------\n:  Class Paramterization Tests (YAML)  :\n----------------------------------------", Console.BLUE
    )
    describe("Basic Parameterized Case Class")
    val inst       = Foo1(false, 19)
    val yaml       = sj.render(inst)
    val comparison = """x: false
                        |b: 19
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[Foo1[Boolean]](yaml))
  }

  test(
    "Non-parameter case class as a parameter - Foo[A](x:A) where A -> Bar (case clas)"
  ) {
    val inst       = Foo1(Bar1("Fred"), 19)
    val yaml       = sj.render(inst)
    val comparison = """x:
                        |  name: Fred
                        |b: 19
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[Foo1[Bar1]](yaml))
  }

  test(
    "Parameterized case class as parameter - Foo[A](x:A) where A -> Bar[Int]"
  ) {
    describe("Advanced Parameterized Case Class")
    val inst       = Foo1(Bar2(123L), 19)
    val yaml       = sj.render(inst)
    val comparison = """x:
                        |  id: 123
                        |b: 19
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[Foo1[Bar2[Long]]](yaml))
  }

  test("Value class as parameter - Foo[A](x:A) where A -> value class") {
    val inst       = Foo1(VC1("Wow"), 19)
    val yaml       = sj.render(inst)
    val comparison = """x: Wow
                        |b: 19
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[Foo1[VC1]](yaml))
  }

  test("Parameterized case class as a parameter - Foo[A](x:Bar[A])") {
    val inst       = Foo2(Bar2(123L), 19)
    val yaml       = sj.render(inst)
    val comparison = """x:
                        |  id: 123
                        |b: 19
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[Foo2[Long]](yaml))
  }

  test(
    "Parameterized case class with parameterized another member - Foo[A](x:Bar[A], y:A)"
  ) {
    val inst       = Foo3(Bar2(List(1, 2, 3)), List(4, 5, 6))
    val yaml       = sj.render(inst)
    val comparison = """x:
                        |  id: [1, 2, 3]
                        |b: [4, 5, 6]
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[Foo3[List[Int]]](yaml))
  }

  test(
    "Class with two parameters, one given one not - Foo[A](x:List[Bar[A, Boolean]])"
  ) {
    val inst = Foo4(
      List(
        Bar3(Map(4 -> "yes", 5    -> "no"), true),
        Bar3(Map(8 -> "yellow", 9 -> "red"), false)
      ),
      Map(1 -> "wow", 2 -> "yup")
    )
    val yaml       = sj.render(inst)
    val comparison = """x:
                        |- id:
                        |    4: yes
                        |    5: no
                        |  isIt: true
                        |- id:
                        |    8: yellow
                        |    9: red
                        |  isIt: false
                        |b:
                        |  1: wow
                        |  2: yup
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[Foo4[Map[Int, String]]](yaml))
  }

  test("Multiple parameters, in order - Foo[A,B](x:Bar[A,B], y:A)") {
    describe("Very Advanced Parameterized Case Class")
    val inst = Foo5(
      List(
        Bar3(Map(4 -> "yes", 5    -> "no"), true),
        Bar3(Map(8 -> "yellow", 9 -> "red"), false)
      ),
      Map(1 -> "wow", 2 -> "yup")
    )
    val yaml       = sj.render(inst)
    val comparison = """x:
                        |- id:
                        |    4: yes
                        |    5: no
                        |  isIt: true
                        |- id:
                        |    8: yellow
                        |    9: red
                        |  isIt: false
                        |b:
                        |  1: wow
                        |  2: yup
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[Foo5[Map[Int, String], Boolean]](yaml))
  }

  test("Multiple parameters, out of order - Foo[A,B,C,D](x:Bar[C,D,A], y:B)") {
    val inst       = Foo6(Bar4(5, 2.5, 'H'), "wow")
    val yaml       = sj.render(inst)
    val comparison = """x:
                        |  id: 5
                        |  thing1: 2.5
                        |  thing2: H
                        |y: wow
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[Foo6[Double, String, Int, Char]](yaml))
  }

  test(
    "Nested multiple parameters, out of order - Foo[A,B,C,D](x:Bar[C,Blah[D,A]], y:B)"
  ) {
    val inst       = Foo7(Bar5(5, Blah(2.5, 'H')), "wow")
    val yaml       = sj.render(inst)
    val comparison = """x:
                        |  id: 5
                        |  blah:
                        |    t: 2.5
                        |    u: H
                        |y: wow
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[Foo7[Double, String, Char, Int]](yaml))
  }
