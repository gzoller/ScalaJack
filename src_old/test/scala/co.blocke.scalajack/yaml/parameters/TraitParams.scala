package co.blocke.scalajack
package yaml
package parameters

import TestUtil._
import munit._
import munit.internal.console

class TraitParams extends FunSuite:

  val sj = ScalaJack(YamlFlavor())

  test("Simple parameters - Foo[A](x:A) where A -> simple type") {
    describe(
      "----------------------------------------\n:  Trait Paramterization Tests (YAML)  :\n----------------------------------------", Console.BLUE
    )
    describe("Basic Parameterized Trait")
    val inst: T1[Boolean] = TFoo1(false, 19)
    val yaml              = sj.render[T1[Boolean]](inst)
    val comparison        = """_hint: co.blocke.scalajack.yaml.parameters.TFoo1
                        |x: false
                        |b: 19
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[T1[Boolean]](yaml))
  }

  test(
    "Non-parameter trait as a parameter - Foo[A](x:A) where A -> Bar (case clas)"
  ) {
    val inst: T1[T2] = TFoo1(TBar1("Fred"), 19)
    val yaml         = sj.render[T1[T2]](inst)
    val comparison   = """_hint: co.blocke.scalajack.yaml.parameters.TFoo1
                        |x:
                        |  _hint: co.blocke.scalajack.yaml.parameters.TBar1
                        |  name: Fred
                        |b: 19
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assert(inst == sj.read[TFoo1[TBar1]](yaml))
  }

  test("Parameterized trait as parameter - Foo[A](x:A) where A -> Bar[Int]") {
    describe("Advanced Parameterized trait")
    val inst: T1[TBar2] = TFoo1(TBar2(true), 19)
    val yaml            = sj.render(inst)
    val comparison      = """_hint: co.blocke.scalajack.yaml.parameters.TFoo1
                        |x:
                        |  thing: true
                        |b: 19
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[T1[TBar2]](yaml))
  }

  test("Value class as parameter - Foo[A](x:A) where A -> value class") {
    val inst: T1[VC1] = TFoo1(VC1("Wow"), 19)
    val yaml          = sj.render(inst)
    val comparison    = """_hint: co.blocke.scalajack.yaml.parameters.TFoo1
                        |x: Wow
                        |b: 19
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[T1[VC1]](yaml))
  }

  test("Parameterized trait as a parameter - Foo[A](x:Bar[A])") {
    val inst: T1[T3[Boolean]] = TFoo1(TBar3(false), 19)
    val yaml                  = sj.render(inst)
    val comparison            = """_hint: co.blocke.scalajack.yaml.parameters.TFoo1
                        |x:
                        |  _hint: co.blocke.scalajack.yaml.parameters.TBar3
                        |  thing: false
                        |b: 19
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[T1[T3[Boolean]]](yaml))
  }

  test(
    "Parameterized trait with parameterized another member - Foo[A](x:Bar[A], y:A)"
  ) {
    val inst: T4[Boolean] = TFoo2(TBar3(false), true)
    val yaml              = sj.render(inst)
    val comparison        = """_hint: co.blocke.scalajack.yaml.parameters.TFoo2
                        |x:
                        |  thing: false
                        |b: true
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[T4[Boolean]](yaml))
  }

  test(
    "Trait with two parameters, one given one not - Foo[A](x:List[Bar[A, Boolean]])"
  ) {
    val inst: T6[Int] = TFoo3(List(TBar4(5, "five"), TBar4(6, "six")))
    val yaml          = sj.render(inst)
    val comparison    = """_hint: co.blocke.scalajack.yaml.parameters.TFoo3
                        |x:
                        |- _hint: co.blocke.scalajack.yaml.parameters.TBar4
                        |  thing1: 5
                        |  thing2: five
                        |- _hint: co.blocke.scalajack.yaml.parameters.TBar4
                        |  thing1: 6
                        |  thing2: six
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[T6[Int]](yaml))
  }

  test("Multiple parameters, in order - Foo[A,B](x:Bar[A,B], y:A)") {
    describe("Very Advanced Parameterized Trait")
    val inst: T7[Long, String] = TFoo4(TBar5(123L, "wow"), 456L)
    val yaml                   = sj.render(inst)
    val comparison             = """_hint: co.blocke.scalajack.yaml.parameters.TFoo4
                        |x:
                        |  _hint: co.blocke.scalajack.yaml.parameters.TBar5
                        |  thing1: 123
                        |  thing2: wow
                        |b: 456
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[T7[Long, String]](yaml))
  }

  test("Multiple parameters, out of order - Foo[A,B,C,D](x:Bar[C,D,A], y:B)") {
    val inst: T8[Char, String, Int, Double] =
      TFoo5(TBar6(5, 2.5, 'H'), "wow")
    val yaml       = sj.render(inst)
    val comparison = """_hint: co.blocke.scalajack.yaml.parameters.TFoo5
                        |x:
                        |  _hint: co.blocke.scalajack.yaml.parameters.TBar6
                        |  pi: 5
                        |  po: 2.5
                        |  pu: H
                        |y: wow
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[T8[Char, String, Int, Double]](yaml))
  }

  test(
    "Nested multiple parameters, out of order - Foo[A,B,C,D](x:Bar[C,Blah[D,A]], y:B)"
  ) {
    val inst: T10[T11[Int, T5[Double, Char]], String] =
      TFoo6(TBlah1(5, TBar7(1.2, 'Z')), "wow")
    val yaml       = sj.render(inst)
    val comparison = """_hint: co.blocke.scalajack.yaml.parameters.TFoo6
                        |x:
                        |  _hint: co.blocke.scalajack.yaml.parameters.TBlah1
                        |  w: 5
                        |  z:
                        |    _hint: co.blocke.scalajack.yaml.parameters.TBar7
                        |    thing1: 1.2
                        |    thing2: Z
                        |y: wow
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[T10[T11[Int, T5[Double, Char]], String]](yaml))
  }