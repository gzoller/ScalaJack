package co.blocke.scalajack
package json
package misc

import ScalaJack.*
import co.blocke.scalajack.json.collections.CarEnum
import co.blocke.scala_reflection.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.*
import scala.util.*
import TestUtil.*

class EnumSpec() extends AnyFunSpec with JsonMatchers:

  describe(colorString("-------------------------------\n:          Enum Tests         :\n-------------------------------", Console.YELLOW)) {
    describe(colorString("+++ Positive Tests +++")) {
      it("Enum as Map key and value must work") {
        val inst = MapHolder[Color, Color](Map(Color.Red -> Color.Blue, Color.Green -> Color.Red))
        val sj = sjCodecOf[MapHolder[Color, Color]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"Red":"Blue","Green":"Red"}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Enum as Map key and value must work (using id)") {
        val inst = MapHolder[Color, Color](Map(Color.Red -> Color.Blue, Color.Green -> Color.Red))
        val sj = sjCodecOf[MapHolder[Color, Color]](JsonConfig.withEnumsAsIds(Nil))
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"0":1,"2":0}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Enumeration (Scale 2) as Map key and value must work") {
        import Permissions.*
        val inst = MapHolder[Permissions, Permissions](Map(Permissions.READ -> Permissions.WRITE, Permissions.EXEC -> Permissions.NONE))
        val sj = sjCodecOf[MapHolder[Permissions, Permissions]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"READ":"WRITE","EXEC":"NONE"}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Enumeration (Scale 2) as Map key and value must work (using id)") {
        import Permissions.*
        val inst = MapHolder[Permissions, Permissions](Map(Permissions.READ -> Permissions.WRITE, Permissions.EXEC -> Permissions.NONE))
        val sj = sjCodecOf[MapHolder[Permissions, Permissions]](JsonConfig.withEnumsAsIds(Nil))
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"0":1,"2":3}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Enumeration (Scala 2) ordinal mutation") {
        import SizeWithType.*
        val inst = SampleEnum(Size.Small, Size.Medium, Size.Large, null, Size.Medium, Little)
        val sj = sjCodecOf[SampleEnum]
        val js = sj.toJson(inst)
        js shouldEqual ("""{"e1":"Small","e2":"Medium","e3":"Large","e4":null,"e5":"Medium","e6":"Little"}""")
        // mutate e5 into an ordinal...
        val js2 = js.replaceAll(""""e5":"Medium"""", """"e5":1""")
        sj.fromJson(js2) shouldEqual (inst)
      }
      it("Java Enumeration as Map key and value must work") {
        val inst = MapHolder[CarEnum, CarEnum](Map(CarEnum.VW -> CarEnum.PORSCHE, CarEnum.PORSCHE -> CarEnum.TOYOTA))
        val sj = sjCodecOf[MapHolder[CarEnum, CarEnum]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"VW":"PORSCHE","PORSCHE":"TOYOTA"}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Java Enumeration as Map key and value must work (using id)") {
        val inst = MapHolder[CarEnum, CarEnum](Map(CarEnum.VW -> CarEnum.PORSCHE, CarEnum.PORSCHE -> CarEnum.TOYOTA))
        val sj = sjCodecOf[MapHolder[CarEnum, CarEnum]](JsonConfig.withEnumsAsIds(Nil))
        val js = sj.toJson(inst)
        val targetJs = RType.of[CarEnum] match
          case t: co.blocke.scala_reflection.rtypes.JavaEnumRType[?] =>
            val valMap = t.values.zipWithIndex.toMap
            s"""{"a":{"${valMap("VW")}":${valMap("PORSCHE")},"${valMap("PORSCHE")}":${valMap("TOYOTA")}}}"""
        js shouldEqual (targetJs)
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Enum/Enumeration mix of enum as value must work") {
        import Permissions.*
        val inst = MapHolder[Color, Permissions](Map(Color.Red -> Permissions.WRITE, Color.Blue -> Permissions.NONE))
        val sj = sjCodecOf[MapHolder[Color, Permissions]](JsonConfig.withEnumsAsIds(List("co.blocke.scalajack.json.misc.Color")))
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"0":"WRITE","1":"NONE"}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Sealed trait enumeration w/case objects must work") {
        val inst = FlavorHolder(Chocolate, null, Map(Bourbon -> "a"), Map("a" -> Vanilla))
        val sj = sjCodecOf[FlavorHolder]
        val js = sj.toJson(inst)
        js should matchJson("""{"f":"Chocolate","f2":null,"f3":{"Bourbon":"a"},"f4":{"a":"Vanilla"}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Sealed trait enumeration w/case class must work") {
        val inst = VehicleHolder(Car(4, "Red"), null, Map("a" -> Truck(18)))
        val sj = sjCodecOf[VehicleHolder]
        val js = sj.toJson(inst)
        js should matchJson("""{"f":{"_hint":"Car","numberOfWheels":4,"color":"Red"},"f2":null,"f4":{"a":{"_hint":"Truck","numberOfWheels":18}}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
    }
    describe(colorString("--- Negative Tests ---")) {
      it("Enum must break - bad value") {
        val sj = sjCodecOf[MapHolder[Color, Color]]
        val js = """{"a":{"Red":"Bogus","Green":"Red"}}"""
        val ex = intercept[java.lang.IllegalArgumentException](sj.fromJson(js))
        ex.getMessage() shouldEqual ("enum co.blocke.scalajack.json.misc.Color has no case with name: Bogus")
      }
      it("Enum must break(using id) - bad value") {
        val sj = sjCodecOf[MapHolder[Color, Color]](JsonConfig.withEnumsAsIds(Nil))
        val js = """{"a":{"0":1,"9":0}}"""
        val ex = intercept[java.util.NoSuchElementException](sj.fromJson(js))
        ex.getMessage() shouldEqual ("enum co.blocke.scalajack.json.misc.Color has no case with ordinal: 9")
      }
    }
  }
