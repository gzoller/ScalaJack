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
      val sj = sjCodecOf[TraitHolder]
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
      val sj = sjCodecOf[TraitHolder](JsonConfig.withTypeHintLabel("ref"))
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
      val sj = sjCodecOf[TraitHolder](JsonConfig.withTypeHintPolicy(TypeHintPolicy.SCRAMBLE_CLASSNAME))
      val js = sj.toJson(inst)
      val diff = parseJValue(js).diff(parseJValue("""{"a":"Start","b":{"_hint":"86999-847-46A","species":"Beta","freshwater":false},"c":{"_hint":"13652-857-33B","temp":101.1},"d":{"_hint":"51470-503-54B","numStreets":25}}"""))
      val diffMap = diff.changed.values.asInstanceOf[Map[String, Map[String, ?]]]
      assert(diffMap("b").contains("_hint") && diffMap("c").contains("_hint")  && diffMap("d").contains("_hint") == true) // ie only the scrambled _hint values are different
      val re = sj.fromJson(js)
      re.a shouldEqual (inst.a)
      re.b shouldEqual (inst.b)
      (re.c.asInstanceOf[Miami].temp == inst.c.asInstanceOf[Miami].temp) shouldEqual (true)
      (re.d.asInstanceOf[CityRoute].numStreets == inst.d.asInstanceOf[CityRoute].numStreets) shouldEqual (true)
    }
    it("Sealed trait with type hint policy USE_ANNOTATION label must work") {
      val inst = TraitHolder(Start, Fish("Beta", false), Miami(101.1), CityRouteImpl(25))
      val sj = sjCodecOf[TraitHolder](JsonConfig.withTypeHintPolicy(TypeHintPolicy.USE_ANNOTATION))
      val js = sj.toJson(inst)
      js should matchJson("""{"a":"Start","b":{"_hint":"flipper","species":"Beta","freshwater":false},"c":{"_hint":"vice","temp":101.1},"d":{"_hint":"CityRoute","numStreets":25}}""")
      val re = sj.fromJson(js)
      re.a shouldEqual (inst.a)
      re.b shouldEqual (inst.b)
      (re.c.asInstanceOf[Miami].temp == inst.c.asInstanceOf[Miami].temp) shouldEqual (true)
      (re.d.asInstanceOf[CityRoute].numStreets == inst.d.asInstanceOf[CityRoute].numStreets) shouldEqual (true)
    }
  }
