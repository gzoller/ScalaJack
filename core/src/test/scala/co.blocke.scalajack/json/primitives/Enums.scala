package co.blocke.scalajack
package json.primitives

import co.blocke.scala_reflection._
import scala.math.BigDecimal
import java.util.UUID
import TestUtil._
import munit._
import munit.internal.console
import co.blocke.scalajack.json.JSON

class Enums() extends FunSuite:

  val sj = co.blocke.scalajack.ScalaJack()
  val sj2 = sj.enumsAsInts()

  test("Enumeration (Scala 2.x) must work (not nullable)") {
    describe("-----------------\n:  Scala Enums  :\n-----------------", Console.BLUE)
    describe("+++ Positive Tests +++")

    import SizeWithType._
    val inst = SampleEnum(Size.Small, Size.Medium, Size.Large, null, Size.Medium, Little)
    val js = sj.render(inst)
    assertEquals(
      """{"e1":"Small","e2":"Medium","e3":"Large","e4":null,"e5":"Medium","e6":"Little"}""".asInstanceOf[JSON],
      js)
    // mutate e5 into an ordinal...
    val js2 = js.asInstanceOf[String].replaceAll(""""e5":"Medium"""", """"e5":1""").asInstanceOf[JSON]
    assertEquals(inst, sj.read[SampleEnum](js2))
  }

  test("Ordinal Enumeration (Scala 2.x) must work (not nullable)") {
    import SizeWithType._
    val inst = SampleEnum(Size.Small, Size.Medium, Size.Large, null, Size.Medium, Little)
    val js = sj2.render(inst)
    assertEquals(
      """{"e1":0,"e2":1,"e3":2,"e4":null,"e5":1,"e6":0}""".asInstanceOf[JSON],
      js)
    // mutate e5 into an ordinal...
    val js2 = js.asInstanceOf[String].replaceAll(""""e5":"Medium"""", """"e5":1""").asInstanceOf[JSON]
    assertEquals(inst, sj.read[SampleEnum](js2))
  }

  test("Enum (Scala 3.x) must work (not nullable)") {
    val inst = TVColors(null, Color.Red)
    val js = sj.render(inst)
    assertEquals(
      """{"color1":null,"color2":"Red"}""".asInstanceOf[JSON],
      js)
    val inst2 = sj.read[TVColors](js)
    assertEquals(inst, inst2)
  }

  test("Ordinal Enum (Scala 3.x) must work (not nullable)") {
    val inst = TVColors(null, Color.Red)
    val js = sj2.render(inst)
    assertEquals(
      """{"color1":null,"color2":0}""".asInstanceOf[JSON],
      js)
    assertEquals(inst, sj2.read[TVColors](js))
  }

  test("""Sealed trait "enums" must work""") {
    val inst = Ride(Car(4,"Red"))
    val js = sj.render(inst)
    assertEquals("""{"wheels":{"numberOfWheels":4,"color":"Red"}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[Ride](js))
  }

  test("""Case object "enums" must work""") {
    val inst = Favorite(Chocolate)
    val js = sj.render(inst)
    assertEquals("""{"flavor":"Chocolate"}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[Favorite](js))
  }

  test("Enumeration (Scala 2.x) must break") {
    describe("--- Negative Tests ---")
    val js =
      """{"e1":"Small","e2":"Bogus","e3":"Large","e4":null,"e5":"Medium","e6":"Little"}""".asInstanceOf[JSON]
    val msg =
      """No value found in enumeration co.blocke.scalajack.json.primitives.Size$ for Bogus
              |{"e1":"Small","e2":"Bogus","e3":"Large","e4":null,"e5":"Medium","e6":"Little"}
              |-------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleEnum](js)
    }
    val js2 =
      """{"e1":"Small","e2":"Medium","e3":"Large","e4":null,"e5":9,"e6":"Little}""".asInstanceOf[JSON]
    val msg2 =
      """No value found in enumeration co.blocke.scalajack.json.primitives.Size$ for 9
               |...Small","e2":"Medium","e3":"Large","e4":null,"e5":9,"e6":"Little}
               |----------------------------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg2){
      sj.read[SampleEnum](js2)
    }
    val js3 =
      """{"e1":"Small","e2":"Medium","e3":"Large","e4":null,"e5":false,"e6":"Little}""".asInstanceOf[JSON]
    val msg3 = """Expected a Number or String here
               |...Small","e2":"Medium","e3":"Large","e4":null,"e5":false,"e6":"Little}
               |----------------------------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg3){
      sj.read[SampleEnum](js3)
    }
  }

  test("Enum (Scala 3.x) must break") {
    val js = """{"color1":null,"color2":"Bogus"}""".asInstanceOf[JSON]
    val msg = """No value found in enumeration co.blocke.scalajack.json.primitives.Color for Bogus
      |{"color1":null,"color2":"Bogus"}
      |------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[TVColors](js)
    }
  }

  //--------------------------------------

  test("Java Enumeration (not nullable) must work") {
    describe("----------------------\n:  Java Enumeration  :\n----------------------", Console.BLUE)
    describe("+++ Positive Tests +++")

    val inst = new JavaEnum()
    inst.setTemp(Temperature.Hot)
    val js = sj.render(inst)
    assertEquals("""{"temp":"Hot"}""".asInstanceOf[JSON], js)
    assertEquals(inst.getTemp, sj.read[JavaEnum](js).getTemp)
  }  

  test("Java Enumeration (not nullable) must fail") {
    describe("--- Negative Tests ---")

    val js = """{"temp":"Bogus"}""".asInstanceOf[JSON]
    interceptMessage[java.lang.IllegalArgumentException]("No enum constant co.blocke.scalajack.Temperature.Bogus"){
      sj.read[JavaEnum](js)
    }
  }
