package co.blocke.scalajack
package json.parameters

import TestUtil._
import munit._
import munit.internal.console
import co.blocke.scalajack.json.JSON
import co.blocke.scala_reflection._

class ClassParams() extends FunSuite:

  val sj = co.blocke.scalajack.ScalaJack()
  
  test("Simple parameters - Foo[A](x:A) where A -> simple type") {
    describe("---------------------------------\n:  Class Paramterization Tests  :\n---------------------------------", Console.BLUE)
    describe("Basic Parameterized Case Class")

    val inst = Foo1(false, 19)
    val js = sj.render(inst)
    assertEquals("""{"x":false,"b":19}""".asInstanceOf[JSON],js)
    assertEquals(inst,sj.read[Foo1[Boolean]](js))
  }

  test("Non-parameter case class as a parameter - Foo[A](x:A) where A -> Bar (case clas)") {
    val inst = Foo1(Bar1("Fred"), 19)
    val js = sj.render(inst)
    assertEquals("""{"x":{"name":"Fred"},"b":19}""".asInstanceOf[JSON],js)
    assertEquals(inst,sj.read[Foo1[Bar1]](js))
  }

  test("Parameterized case class as parameter - Foo[A](x:A) where A -> Bar[Int]") {
    describe("Advanced Parameterized Case Class")

    val inst = Foo1(Bar2(123L), 19)
    val js = sj.render(inst)
    assertEquals("""{"x":{"id":123},"b":19}""".asInstanceOf[JSON],js)
    assertEquals(inst,sj.read[Foo1[Bar2[Long]]](js))
  }

  test("Value class as parameter - Foo[A](x:A) where A -> value class") {
    val inst = Foo1(VC1("Wow"), 19)
    val js = sj.render(inst)
    assertEquals("""{"x":"Wow","b":19}""".asInstanceOf[JSON],js)
    assertEquals(inst,sj.read[Foo1[VC1]](js))
  }

  test("Parameterized case class as a parameter - Foo[A](x:Bar[A])") {
    val inst = Foo2(Bar2(123L), 19)
    val js = sj.render(inst)
    assertEquals("""{"x":{"id":123},"b":19}""".asInstanceOf[JSON],js)
    assertEquals(inst,sj.read[Foo2[Long]](js))
  }

  test("Parameterized case class with parameterized another member - Foo[A](x:Bar[A], y:A)") {
    val inst = Foo3(Bar2(List(1, 2, 3)), List(4, 5, 6))
    val js = sj.render(inst)
    assertEquals("""{"x":{"id":[1,2,3]},"b":[4,5,6]}""".asInstanceOf[JSON],js)
    assertEquals(inst,sj.read[Foo3[List[Int]]](js))
  }

  test("Class with two parameters, one given one not - Foo[A](x:List[Bar[A, Boolean]])") {
    val inst = Foo4(
      List(
        Bar3(Map(4 -> "yes", 5 -> "no"), true),
        Bar3(Map(8 -> "yellow", 9 -> "red"), false)
      ),
      Map(1 -> "wow", 2 -> "yup")
    )
    val js = sj.render(inst)
    assertEquals(
      """{"x":[{"id":{"4":"yes","5":"no"},"isIt":true},{"id":{"8":"yellow","9":"red"},"isIt":false}],"b":{"1":"wow","2":"yup"}}""".asInstanceOf[JSON],js)
    assertEquals(inst,sj.read[Foo4[Map[Int, String]]](js))
  }

  test("Multiple parameters, in order - Foo[A,B](x:Bar[A,B], y:A)") {
    describe("Very Advanced Parameterized Case Class")

    val inst = Foo5(
      List(
        Bar3(Map(4 -> "yes", 5 -> "no"), true),
        Bar3(Map(8 -> "yellow", 9 -> "red"), false)
      ),
      Map(1 -> "wow", 2 -> "yup")
    )
    val js = sj.render(inst)
    assertEquals(
      """{"x":[{"id":{"4":"yes","5":"no"},"isIt":true},{"id":{"8":"yellow","9":"red"},"isIt":false}],"b":{"1":"wow","2":"yup"}}""".asInstanceOf[JSON],js)
    assertEquals(inst,sj.read[Foo5[Map[Int, String], Boolean]](js))
  }

  test("Multiple parameters, out of order - Foo[A,B,C,D](x:Bar[C,D,A], y:B)") {
    val inst = Foo6(Bar4(5, 2.5, 'H'), "wow")
    val js = sj.render(inst)
    assertEquals("""{"x":{"id":5,"thing1":2.5,"thing2":"H"},"y":"wow"}""".asInstanceOf[JSON],js)
    assertEquals(inst,sj.read[Foo6[Double, String, Int, Char]](js))
  }

  test("Nested multiple parameters, out of order - Foo[A,B,C,D](x:Bar[C,Blah[D,A]], y:B)") {
    val inst = Foo7(Bar5(5, Blah(2.5, 'H')), "wow")
    val js = sj.render(inst)
    assertEquals("""{"x":{"id":5,"blah":{"t":2.5,"u":"H"}},"y":"wow"}""".asInstanceOf[JSON],js)
    assertEquals(inst,sj.read[Foo7[Double, String, Char, Int]](js))
  }