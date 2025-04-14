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
      val m2 = Lift("bigLift", 2000)
      val m3 = Drill("bigDrill", 24)
      val m4 = Swing("swingLow", None, true)
      val inst = MachineHolder(m1, m2, m3, m4)
      val sj = sjCodecOf[MachineHolder]
      val js = sj.toJson(inst)
      // m1 and m2 need the hint--their field signature is identical. m3 and m4 have unique signatures
      js should matchJson("""{"m1":{"_hint":"Press","name":"bigPress","lbs":1000},"m2":{"_hint":"Lift","name":"bigLift","lbs":2000},"m3":{"name":"bigDrill","numBits":24},"m4":{"name":"swingLow","isBig":true}}""")
      // TODO: Read this back in!!!
    }
  }
