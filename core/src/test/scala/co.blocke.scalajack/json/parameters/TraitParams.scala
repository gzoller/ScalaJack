package co.blocke.scalajack
package json.parameters

import TestUtil._
import munit._
import munit.internal.console
import co.blocke.scalajack.json.JSON
import co.blocke.scala_reflection._

class TraitParams() extends FunSuite:

  val sj = co.blocke.scalajack.ScalaJack()
  
  test("Simple parameters - Foo[A](x:A) where A -> simple type") {
    describe("---------------------------------\n:  Trait Paramterization Tests  :\n---------------------------------", Console.BLUE)
    describe("Basic Parameterized Trait")

    val inst: T1[Boolean] = TFoo1(false, 19)
    val js = sj.render[T1[Boolean]](inst)
    assertEquals("""{"_hint":"co.blocke.scalajack.json.parameters.TFoo1","x":false,"b":19}""".asInstanceOf[JSON],js)
    assertEquals(inst,sj.read[T1[Boolean]](js))
  }

  test("Non-parameter trait as a parameter - Foo[A](x:A) where A -> Bar (case class)") {
    val inst: T1[T2] = TFoo1(TBar1("Fred"), 19)
    val js = sj.render[T1[T2]](inst)
    assertEquals(
      """{"_hint":"co.blocke.scalajack.json.parameters.TFoo1","x":{"_hint":"co.blocke.scalajack.json.parameters.TBar1","name":"Fred"},"b":19}""".asInstanceOf[JSON],js)
    assert(inst == sj.read[TFoo1[TBar1]](js))
  }

  /*
  test("Parameterized trait as parameter - Foo[A](x:A) where A -> Bar[Int]") {
    describe("Advanced Parameterized trait")

    val inst: T1[TBar2] = TFoo1(TBar2(true), 19)
    val js = sj.render(inst)
    assertEquals(
      """{"_hint":"co.blocke.scalajack.json.parameters.TFoo1","x":{"thing":true},"b":19}""".asInstanceOf[JSON],js)
    assert(inst == sj.read[T1[TBar2]](js))
  }
  
  test("Value class as parameter - Foo[A](x:A) where A -> value class") {
    val inst: T1[VC1] = TFoo1(VC1("Wow"), 19)
    val js = sj.render(inst)
    assertEquals("""{"_hint":"co.blocke.scalajack.json.parameters.TFoo1","x":"Wow","b":19}""".asInstanceOf[JSON],js)
    assert(inst == sj.read[T1[VC1]](js))
  }

  test("Parameterized trait as a parameter - Foo[A](x:Bar[A])") {
    val inst: T1[T3[Boolean]] = TFoo1(TBar3(false), 19)
    val js = sj.render(inst)
    assertEquals(
      """{"_hint":"co.blocke.scalajack.json.parameters.TFoo1","x":{"_hint":"co.blocke.scalajack.json.parameters.TBar3","thing":false},"b":19}""".asInstanceOf[JSON],js)
    assert(inst == sj.read[T1[T3[Boolean]]](js))
  }

  test("Parameterized trait with parameterized another member - Foo[A](x:Bar[A], y:A)") {
    val inst: T4[Boolean] = TFoo2(TBar3(false), true)
    val js = sj.render(inst)
    assertEquals(
      """{"_hint":"co.blocke.scalajack.json.parameters.TFoo2","x":{"thing":false},"b":true}""".asInstanceOf[JSON],js)
    assert(inst == sj.read[T4[Boolean]](js))
  }

  test("Trait with two parameters, one given one not - Foo[A](x:List[Bar[A, Boolean]])") {
    val inst: T6[Int] = TFoo3(List(TBar4(5, "five"), TBar4(6, "six")))
    val js = sj.render(inst)
    assertEquals(
      """{"_hint":"co.blocke.scalajack.json.parameters.TFoo3","x":[{"_hint":"co.blocke.scalajack.json.parameters.TBar4","thing1":5,"thing2":"five"},{"_hint":"co.blocke.scalajack.json.parameters.TBar4","thing1":6,"thing2":"six"}]}""".asInstanceOf[JSON],js)
    assert(inst == sj.read[T6[Int]](js))
  }

  test("Multiple parameters, in order - Foo[A,B](x:Bar[A,B], y:A)") {
    describe("Very Advanced Parameterized Trait")

    val inst: T7[Long, String] = TFoo4(TBar5(123L, "wow"), 456L)
    val js = sj.render(inst)
    assertEquals(
      """{"_hint":"co.blocke.scalajack.json.parameters.TFoo4","x":{"_hint":"co.blocke.scalajack.json.parameters.TBar5","thing1":123,"thing2":"wow"},"b":456}""".asInstanceOf[JSON],js)
    assert(inst == sj.read[T7[Long, String]](js))
  }

  test("Multiple parameters, out of order - Foo[A,B,C,D](x:Bar[C,D,A], y:B)") {
    val inst: T8[Char, String, Int, Double] =
      TFoo5(TBar6(5, 2.5, 'H'), "wow")
    val js = sj.render(inst)
    assertEquals(
      """{"_hint":"co.blocke.scalajack.json.parameters.TFoo5","x":{"_hint":"co.blocke.scalajack.json.parameters.TBar6","pi":5,"po":2.5,"pu":"H"},"y":"wow"}""".asInstanceOf[JSON],js)
    assert(inst == sj.read[T8[Char, String, Int, Double]](js))
  }

  test("Multiple parameters, out of order - Foo[A,B,C,D](x:Bar[C,D,A], y:B)") {
    val inst: T8[Char, String, Int, Double] =
      TFoo5(TBar6(5, 2.5, 'H'), "wow")
    val js = sj.render(inst)
    assertEquals(
      """{"_hint":"co.blocke.scalajack.json.parameters.TFoo5","x":{"_hint":"co.blocke.scalajack.json.parameters.TBar6","pi":5,"po":2.5,"pu":"H"},"y":"wow"}""".asInstanceOf[JSON],js)
    assert(inst == sj.read[T8[Char, String, Int, Double]](js))
  }

  test("Nested multiple parameters, out of order - Foo[A,B,C,D](x:Bar[C,Blah[D,A]], y:B)") {
    val rt = RType.of[T10[T11[Int, T5[Double, Char]], String]]
    val inst: T10[T11[Int, T5[Double, Char]], String] = TFoo6(TBlah1(5, TBar7(1.2, 'Z')), "wow")
    val js = sj.render(inst)
    assertEquals(
      """{"_hint":"co.blocke.scalajack.json.parameters.TFoo6","x":{"_hint":"co.blocke.scalajack.json.parameters.TBlah1","w":5,"z":{"_hint":"co.blocke.scalajack.json.parameters.TBar7","thing1":1.2,"thing2":"Z"}},"y":"wow"}""".asInstanceOf[JSON],js)
    val z = sj.read[T10[T11[Int, T5[Double, Char]], String]](js)
    assertEquals(inst, sj.read[T10[T11[Int, T5[Double, Char]], String]](js))
  }
  */

  /* Performance tests
  var rtx: RType = null
  var jsx: JSON = null.asInstanceOf[JSON]
  var zx: T10[T11[Int, T5[Double, Char]], String] = null
  test("a") {
    rtx = Reflector.reflectOn[T10[T11[Int, T5[Double, Char]], String]]
  }
  test("b") {
    val inst: T10[T11[Int, T5[Double, Char]], String] = TFoo6(TBlah1(5, TBar7(1.2, 'Z')), "wow")
    jsx = sj.render(inst)
  }
  test("c") {
    zx = sj.read[T10[T11[Int, T5[Double, Char]], String]](jsx)
  }
  test("a.2") {
    rtx = Reflector.reflectOn[T10[T11[Int, T5[Double, Char]], String]]
  }
  test("b.2") {
    val inst: T10[T11[Int, T5[Double, Char]], String] = TFoo6(TBlah1(5, TBar7(1.2, 'Z')), "wow")
    jsx = sj.render(inst)
  }
  test("c.2") {
    zx = sj.read[T10[T11[Int, T5[Double, Char]], String]](jsx)
  }
  test("Z") {
    val inst: Pet[Boolean] = Dog("Fido",4,true)
    val js = sj.render(inst)
    sj.read[Pet[Boolean]](js)
  }
  */

