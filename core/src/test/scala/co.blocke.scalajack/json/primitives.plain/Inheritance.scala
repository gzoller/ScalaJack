package co.blocke.scalajack
package json
package primitives.plain

import org.scalatest.Matchers
import org.scalatest.funspec.AnyFunSpec
import JsonMatcher._

class Inheritance() extends AnyFunSpec with Matchers {

  val sj = ScalaJack()

  describe(
    "-------------------------------------\n:  Inheritance Tests (Plain Class)  :\n-------------------------------------"
  ) {
      describe("Scala Plain") {
        it("Simple class inheritance must work (all fields present)") {
          val js =
            """{"uno":"foo","foobar":99,"dontForget":12,"three":false,"quatro":12.34,"foo":25,"extra":"bar"}"""
          val simple = sj.read[InheritSimpleChild](js)
          simple.one should be("foo")
          simple.extra should be("bar")
          simple.foo should be(25)
          simple.two should be(99)
          simple.three should be(false)
          simple.four should be(12.34)
          // Need this matching because JSON order is often different
          parseJValue(js) should matchJson(parseJValue(sj.render(simple)))
        }
        it("MapName, and Ignore annotations must be inherited properly") {
          val adapter = sj.taCache
            .typeAdapterOf[InheritSimpleChild]
            .asInstanceOf[typeadapter.PlainClassTypeAdapter[_]]
          adapter.dbKeys.map(f => (f.name, f.dbKeyIndex)) should be(
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
          parseJValue(js) should matchJson(
            parseJValue(
              """{"extra":"thing1","uno":"thing2","foo":39,"dontForget":9,"quatro":0.1,"three":true,"foobar":5}"""
            )
          )
        }
        it("Optional annotation must be inherited properly") {
          val js =
            """{"extra":"bar","foo":25,"uno":"something","dontForget":12,"quatro":12.34,"foobar":99}"""
          val inst = sj.read[InheritSimpleChild](js)
          inst.one should be("something")
          inst.three should be(true)
        }
        it("With type parameter") {
          val js = """{"thing":5, "item": 15, "cosa": 99}"""
          val inst = sj.read[ParamChild[Int]](js)
          inst.thing should be(5)
          inst.item should be(15)
          inst.cosa should be(99)
          sj.render(inst) should be("""{"thing":5,"cosa":99,"item":15}""")
        }
        it("With type member (as part of a trait)") {
          val inst = new WrapTrait[Flower]()
          val flower = new Flower(5, 6)
          inst.rose = flower
          val js = sj.render(inst)
          js should be(
            """{"flower":"co.blocke.scalajack.json.primitives.plain.Flower","rose":{"thing":5,"other":6}}"""
          )
          val inst2 = sj.read[WrapTrait[TraitBase]](js)
          (inst2.rose.thing == flower.thing && inst2.rose.other == flower.other) should be(
            true
          )
        }
      }
      describe("Scala Plain Negative") {
        it("Must catch missing/required var") {
          val js =
            """{"extra":"bar","foo":25,"dontForget":12,"uno":"something","quatro":12.34}"""
          val msg = """Class InheritSimpleChild missing required fields: foobar
                    |...,"dontForget":12,"uno":"something","quatro":12.34}
                    |----------------------------------------------------^""".stripMargin
          the[ScalaJackError] thrownBy sj.read[InheritSimpleChild](js) should have message msg
        }
        it("Must catch missing/required constructor field (with newline)") {
          val js =
            """{"extra":"bar","foo":25,"dontForget":12,"quatro"
            |:12.34,"foobar":99}""".stripMargin
          val msg =
            """Class InheritSimpleChild missing required constructor fields: uno
                    |...o":25,"dontForget":12,"quatro"~:12.34,"foobar":99}
                    |----------------------------------------------------^""".stripMargin
          the[ScalaJackError] thrownBy sj.read[InheritSimpleChild](js) should have message msg
        }
        it("Must catch missing/required getter/setter field") {
          val js =
            """{"extra":"bar","foo":25,"uno":"something","quatro":12.34,"foobar":99}"""
          val msg =
            """Class InheritSimpleChild missing required fields: dontForget
                    |...":25,"uno":"something","quatro":12.34,"foobar":99}
                    |----------------------------------------------------^""".stripMargin
          the[ScalaJackError] thrownBy sj.read[InheritSimpleChild](js) should have message msg
        }
        it("Must fail non-val constructor field") {
          val f = new Fail4(1, 2)
          the[java.lang.IllegalStateException] thrownBy sj.render(f) should have message """ScalaJack doesn't support non-val constructor fields (they can't be read by reflection)"""
        }
      }
      describe("Java Plain") {
        it(
          "Simple class inheritance must work (all fields present) including MapName and Ignore"
        ) {
            val js = """{"three":3,"dos":1}"""
            val simple = sj.read[JavaSimpleChild](js)
            simple.getTwo should be(1)
            simple.getThree should be(3)
            simple.getBogus should be(-1)
            sj.render(simple) should be(js)
          }
        it("Optional annotation must be inherited properly") {
          val js = """{"dos":1}"""
          val simple = sj.read[JavaSimpleChild](js)
          simple.getTwo should be(1)
          simple.getThree should be(-10)
          sj.render(simple) should be("""{"three":-10,"dos":1}""")
        }
      }
    }
}
