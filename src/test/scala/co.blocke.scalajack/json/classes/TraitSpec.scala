package co.blocke.scalajack
package json
package classes

import ScalaJack.*
import co.blocke.scala_reflection.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.*
import scala.util.*
import TestUtil.*

import java.util.UUID

class TraitSpec() extends AnyFunSpec with JsonMatchers:
  opaque type phone = String

  describe(colorString("-------------------------------\n:         Trait Tests         :\n-------------------------------", Console.YELLOW)) {
    it("Sealed trait with case objects and case classes must work") {
      val inst = TraitHolder(Start, Fish("Beta", false), Miami(101.1), CityRouteImpl(99))
      val sj = sjCodecOf[TraitHolder](SJConfig.preferTypeHints)
      val js = sj.toJson(inst)
      js should matchJson("""{"a":"Start","b":{"_hint":"Fish","species":"Beta","freshwater":false},"c":{"_hint":"Miami","temp":101.1},"d":{"_hint":"CityRoute","numStreets":99}}""")
      val re = sj.fromJson(js)
      re.a shouldEqual (inst.a)
      re.b shouldEqual (inst.b)
      (re.c.asInstanceOf[Miami].temp == inst.c.asInstanceOf[Miami].temp) shouldEqual (true)
      (re.d.asInstanceOf[CityRoute].numStreets == inst.d.asInstanceOf[CityRoute].numStreets) shouldEqual (true)
    }
    it("Sealed trait with modified type hint label must work") {
      val inst = TraitHolder(Start, Fish("Beta", false), Miami(101.1), CityRouteImpl(25))
      val sj = sjCodecOf[TraitHolder](SJConfig.withTypeHintLabel("ref").preferTypeHints)
      val js = sj.toJson(inst)
      js should matchJson("""{"a":"Start","b":{"ref":"Fish","species":"Beta","freshwater":false},"c":{"ref":"Miami","temp":101.1},"d":{"ref":"CityRoute","numStreets":25}}""")
      val re = sj.fromJson(js)
      re.a shouldEqual (inst.a)
      re.b shouldEqual (inst.b)
      (re.c.asInstanceOf[Miami].temp == inst.c.asInstanceOf[Miami].temp) shouldEqual (true)
      (re.d.asInstanceOf[CityRoute].numStreets == inst.d.asInstanceOf[CityRoute].numStreets) shouldEqual (true)
    }
    it("Sealed trait with type hint policy SCRAMBLE_CLASSNAME label must work") {
      val inst = TraitHolder(Start, Fish("Beta", false), Miami(101.1), CityRouteImpl(25))
      val sj = sjCodecOf[TraitHolder](SJConfig.withTypeHintPolicy(TypeHintPolicy.SCRAMBLE_CLASSNAME).preferTypeHints)
      val js = sj.toJson(inst)
      val diff = parseJValue(js).diff(parseJValue("""{"a":"Start","b":{"_hint":"86999-847-46A","species":"Beta","freshwater":false},"c":{"_hint":"13652-857-33B","temp":101.1},"d":{"_hint":"51470-503-54B","numStreets":25}}"""))
      val diffMap = diff.changed.values.asInstanceOf[Map[String, Map[String, ?]]]
      assert(diffMap("b").contains("_hint") && diffMap("c").contains("_hint") && diffMap("d").contains("_hint")) // ie only the scrambled _hint values are different
      val re = sj.fromJson(js)
      re.a shouldEqual (inst.a)
      re.b shouldEqual (inst.b)
      (re.c.asInstanceOf[Miami].temp == inst.c.asInstanceOf[Miami].temp) shouldEqual (true)
      (re.d.asInstanceOf[CityRoute].numStreets == inst.d.asInstanceOf[CityRoute].numStreets) shouldEqual (true)
    }
    it("Sealed trait with type hint policy USE_ANNOTATION label must work") {
      val inst = TraitHolder(Start, Fish("Beta", false), Miami(101.1), CityRouteImpl(25))
      val sj = sjCodecOf[TraitHolder](SJConfig.withTypeHintPolicy(TypeHintPolicy.USE_ANNOTATION).preferTypeHints)
      val js = sj.toJson(inst)
      js should matchJson("""{"a":"Start","b":{"_hint":"flipper","species":"Beta","freshwater":false},"c":{"_hint":"vice","temp":101.1},"d":{"_hint":"CityRoute","numStreets":25}}""")
      val re = sj.fromJson(js)
      re.a shouldEqual (inst.a)
      re.b shouldEqual (inst.b)
      (re.c.asInstanceOf[Miami].temp == inst.c.asInstanceOf[Miami].temp) shouldEqual (true)
      (re.d.asInstanceOf[CityRoute].numStreets == inst.d.asInstanceOf[CityRoute].numStreets) shouldEqual (true)
    }
    it("Complex trait relationships must work") {
      val inst: ComplexPerson = Employee(Painter(5, Sports(1.2, 'Z')), Car(4))
      val sj = sjCodecOf[ComplexPerson](SJConfig.preferTypeHints)
      val js = sj.toJson(inst)
      js should matchJson("""{"_hint":"Employee","who":{"_hint":"Painter","instrument":5,"effort":{"_hint":"Sports","thing1":1.2,"thing2":"Z"}},"org":{"_hint":"Car","passengers":4}}""")
      sj.fromJson(js) shouldEqual (inst)
    }
    it("Avoiding type hints works properly (no hints unless needed)") {
      val m1 = Press("bigPress", 1000)
      val m2 = Lift("bigLift", 2000, false)
      val m3 = Drill("bigDrill", 24)
      val m4 = Swing("swingLow", None, true)
      val inst = MachineHolder(m1, m2, m3, m4)
      val sj = sjCodecOf[MachineHolder]
      val js = sj.toJson(inst)
      js should matchJson("""{"m1":{"name":"bigPress","lbs":1000},"m2":{"name":"bigLift","lbs":2000,"foo":false},"m3":{"name":"bigDrill","numBits":24},"m4":{"name":"swingLow","isBig":true}}""")
      sj.fromJson(js) shouldEqual (inst)
    }
    it("Always generates type hints when needed") {
      val m1 = Press2("bigPress", Some(1000))
      val m2 = Lift2("bigLift", None)
      val m3 = Drill2("bigDrill", None, 3)
      val inst = MachineHolder2(m1, m2, m3)
      val sj = sjCodecOf[MachineHolder2]
      val js = sj.toJson(inst)
      // Note: m3 didn't need the hint, but m1 and m2 did--and they were generated regardless of a preference not to
      js should matchJson("""{"m1":{"_hint":"Press2","name":"bigPress","lbs":1000},"m2":{"_hint":"Lift2","name":"bigLift"},"m3":{"name":"bigDrill","numBits":3}}""")
      sj.fromJson(js) shouldEqual (inst)
    }
    it("Fails when a class that needs a hint doesn't have one") {
      val js = """{"m1":{"_hint":"Press2","name":"bigPress","lbs":1000},"m2":{"name":"bigLift"},"m3":{"name":"bigDrill","numBits":3}}"""
      val msg =
        """Class in trait co.blocke.scalajack.json.classes.Machine2 with parsed fields [name] needed a type hint but none was found (ambiguous) at position [59]
          |...int":"Press2","name":"bigPress","lbs":1000},"m2":{"name":"bigLift"},"m3":{"n...
          |----------------------------------------------------^""".stripMargin
      val ex = intercept[JsonParseError](sjCodecOf[MachineHolder2].fromJson(js))
      ex.show shouldEqual msg
    }
    it("Complext test with nested trait and empty unique field") {
      val a = L0A(5, None, true)
      val b = L0B(Some("wow"), "abc", None)
      val c = L0C(Some(3), List(1, 2, 3))
      val r = L1R("blather", None, a)
      val s = L1S(123L, "Miguel")
      val q = L1Q("aaa", 100, b)
      val x = L1X("bbb", 99, c)
      val inst = ComplexHolder(r, s, q, x)
      val sj = sjCodecOf[ComplexHolder]
      val js = sj.toJson(inst)
      js should matchJson(
        """{"c1":{"blather":"blather","l0":{"x":5,"y":true}},"c2":{"id":123,"nombre":"Miguel"},"c3":{"_hint":"L1Q","name":"aaa","age":100,"l0":{"name":"wow","id":"abc"}},"c4":{"_hint":"L1X","name":"bbb","age":99,"l0":{"id":3,"extra":[1,2,3]}}}"""
      )
      sj.fromJson(js) shouldEqual (inst)
    }
    it("Self-referencing must work") {
      val inst = OuterImpl("foo", 55, List(OuterImpl("bar", 100, Nil)))
      val sj = sjCodecOf[Outer]
      val js = sj.toJson(inst)
      js should matchJson(
        """{"name":"foo","num":55,"stuff":[{"name":"bar","num":100,"stuff":[]}]}"""
      )
      sj.fromJson(js) shouldEqual (inst)
    }
  }
