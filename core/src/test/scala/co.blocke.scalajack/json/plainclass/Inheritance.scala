package co.blocke.scalajack
package json
package plainclass

import co.blocke.scalajack.model.ClassNameHintModifier
import co.blocke.scala_reflection._
import TestUtil._
import munit._
import munit.internal.console
import co.blocke.scalajack.json.JSON
import JsonMatcher._


class Inheritance() extends FunSuite:

  val sj = co.blocke.scalajack.ScalaJack()

  test("Simple class inheritance must work (all fields present)") {
    describe(
      "-------------------------------------\n:  Inheritance Tests (Plain Class)  :\n-------------------------------------", Console.BLUE
    )
    describe("Scala Plain")

    val js =
      """{"uno":"foo","foobar":99,"dontForget":12,"three":false,"quatro":12.34,"foo":25,"extra":"bar"}""".asInstanceOf[JSON]
    val simple = sj.read[InheritSimpleChild](js)
    assertEquals(simple.one, "foo")
    assertEquals(simple.extra, "bar")
    assertEquals(simple.foo, 25)
    assertEquals(simple.two, 99)
    assertEquals(simple.three, false)
    assertEquals(simple.four, 12.34)
    // Need this matching because JSON order is often different
    assert(JsonMatcher.jsonMatches(js, sj.render(simple)))
  }

  test("MapName, and Ignore annotations must be inherited properly") {
    val adapter = sj.taCache
      .typeAdapterOf[InheritSimpleChild]
      .asInstanceOf[co.blocke.scalajack.typeadapter.classes.NonCaseClassTypeAdapter[_]]
    assertEquals( adapter.dbKeys.map(f => (f.name, f.dbKeyIndex)),
      List(
        ("uno", Some(0)),
        ("foobar", Some(1)),
        ("quatro", Some(2)),
        ("foo", Some(99))
      )
    )
    val inst = new InheritSimpleChild("thing1", "thing2")
    val js = sj.render(inst)
    // Need this matching because JSON order is often different
    assert(JsonMatcher.jsonMatches(js, """{"extra":"thing1","uno":"thing2","foo":39,"dontForget":9,"quatro":0.1,"three":true,"foobar":5}""".asInstanceOf[JSON]))
  }

  test("With type parameter") {
    val js = """{"thing":5, "item": 15, "cosa": 99}""".asInstanceOf[JSON]
    val inst = sj.read[ParamChild[Int]](js)
    assertEquals(inst.thing, 5)
    assertEquals(inst.item, 15)
    assertEquals(inst.cosa, 99)
    assert(JsonMatcher.jsonMatches(sj.render(inst), """{"thing":5,"cosa":99,"item":15}""".asInstanceOf[JSON]))
  }

  test("With type member (as part of a trait)") {
    val inst = new WrapTrait[TraitBase]()
    val flower = new Flower(5, 6)
    inst.rose = flower
    val js = sj.render(inst)
    assertEquals(js,
      """{"flower":"co.blocke.scalajack.json.plainclass.Flower","rose":{"thing":5,"other":6}}""".asInstanceOf[JSON]
    )
    val inst2 = sj.read[WrapTrait[TraitBase]](js)
    assert(inst2.rose.thing == flower.thing && inst2.rose.other == flower.other)
  }

  test("Must catch missing/required var") {
    describe("Scala Plain Negative") 

    val js =
      """{"extra":"bar","foo":25,"dontForget":12,"uno":"something","quatro":12.34}""".asInstanceOf[JSON]
    val msg = """Class co.blocke.scalajack.json.plainclass.InheritSimpleChild missing required fields: foobar
              |...,"dontForget":12,"uno":"something","quatro":12.34}
              |----------------------------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[InheritSimpleChild](js)
    }
  }

  test("Must catch missing/required constructor field (with newline)") {
    val js =
      """{"extra":"bar","foo":25,"dontForget":12,"quatro":12.34,"foobar":99}""".asInstanceOf[JSON]
    val msg =
      """Class co.blocke.scalajack.json.plainclass.InheritSimpleChild missing required fields: uno
              |...oo":25,"dontForget":12,"quatro":12.34,"foobar":99}
              |----------------------------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[InheritSimpleChild](js)
    }
  }

  test("Must catch missing/required getter/setter field") {
    val js =
      """{"extra":"bar","foo":25,"uno":"something","quatro":12.34,"foobar":99}""".asInstanceOf[JSON]
    val msg =
      """Class co.blocke.scalajack.json.plainclass.InheritSimpleChild missing required fields: dontForget
              |...":25,"uno":"something","quatro":12.34,"foobar":99}
              |----------------------------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[InheritSimpleChild](js)
    }
  }

  // NOTE: We can't test this because the exception is tossed at compile-time.  (It will indeed throw this exception, however.)
  //----------
  // test("Must fail non-val constructor field") {
  //   val f = new Fail4(1, 2)
  //   interceptMessage[co.blocke.scala_reflection.ReflectException]("""Class [co.blocke.scalajack.json.plainclass.Fail4]: Non-case class constructor arguments must all be 'val'"""){
  //     sj.render(f)
  //   }
  // }

  test(
    "Simple class inheritance must work (all fields present) including MapName and Ignore"
  ) {
    describe("Java Plain") 

    val js = """{"three":3,"dos":1}""".asInstanceOf[JSON]
    val simple = sj.read[JavaSimpleChild](js)
    assertEquals(simple.getTwo, 1)
    assertEquals(simple.getThree, 3)
    assertEquals(simple.getBogus, -1)
    assertEquals(sj.render(simple), js)
  }

  test("Optional annotation must be inherited properly") {
    val js = """{"dos":1}""".asInstanceOf[JSON]
    val simple = sj.read[JavaSimpleChild](js)
    assertEquals(simple.getTwo, 1)
    assertEquals(simple.getThree, -10)
    assertEquals(sj.render(simple), """{"three":-10,"dos":1}""".asInstanceOf[JSON])
  }