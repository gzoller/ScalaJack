package co.blocke.scalajack
package json.structures

import co.blocke.scala_reflection._
import TestUtil._
import munit._
import munit.internal.console
import co.blocke.scalajack.json.JSON


class Eithers() extends FunSuite:

  val sj = co.blocke.scalajack.ScalaJack()
  
  test("Left - two class types") {
    describe("------------------\n:  Either Tests  :\n------------------",Console.BLUE)
    describe("+++ Positive Tests +++")

    val inst: Either[Parrot, DumpTruck] = Left(Parrot("blue"))
    try {
      val js = sj.render(inst)
      assertEquals("""{"color":"blue"}""".asInstanceOf[JSON],js)
      assertEquals(inst,sj.read[Either[Parrot, DumpTruck]](js))
    } catch {
      case t: Throwable => println("Boom")
    }
  }

  test("Right - two class types") {
    val inst: Either[Parrot, DumpTruck] = Right(DumpTruck(axles = 2))
    val js = sj.render(inst)
    assertEquals("""{"axles":2}""".asInstanceOf[JSON],js)
    assertEquals(inst,sj.read[Either[Parrot, DumpTruck]](js))
  }

  test("Left - class type and scalar type") {
    val inst: Either[Parrot, String] = Left(Parrot("red"))
    val js = sj.render(inst)
    assertEquals("""{"color":"red"}""".asInstanceOf[JSON],js)
    assert(inst == sj.read[Either[Parrot, DumpTruck]](js))
  }

  test("Right - class type and scalar type") {
    val inst = EitherHolder[Parrot, String](Right("quack"))
    val js = sj.render(inst)
    assertEquals("""{"either":"quack"}""".asInstanceOf[JSON],js)
    assertEquals(inst,sj.read[EitherHolder[Parrot, String]](js))
  }

  test("Either is null") {
    val inst: Either[Parrot, String] = null
    val js = sj.render(inst)
    assertEquals("null".asInstanceOf[JSON],js)
    assert(null == sj.read[Either[Parrot, String]](js))
  }

  test("Different classes with identical fields--favor Right") {
    val js = """{"numLegs":4}""".asInstanceOf[JSON]
    assertEquals(sj.read[Either[Chair, Table]](js),Right(Table(4)))
  }

  test("Handles traits - Right") {
    val inst = EitherHolder[String, Pet](Right(Dog("Fido", 13)))
    val js = sj.render(inst)
    assertEquals(
      """{"either":{"_hint":"co.blocke.scalajack.json.structures.Dog","name":"Fido","kind":13}}""".asInstanceOf[JSON],js)
    assertEquals(inst,sj.read[EitherHolder[String, Pet]](js))
  }

  test("Handles traits - Left") {
    val inst = EitherHolder[Pet, String](Left(Dog("Fido", 13)))
    val js = sj.render(inst)
    assertEquals(
      """{"either":{"_hint":"co.blocke.scalajack.json.structures.Dog","name":"Fido","kind":13}}""".asInstanceOf[JSON],js)
    assertEquals(inst,sj.read[EitherHolder[Pet, String]](js))
  }

  test("Same instance Left and Right") {
    describe("--- Negative Tests ---",Console.BLUE)
    val js = "\"foo\""
    val msg =
      """Types java.lang.String and java.lang.String are not mutually exclusive""".stripMargin
    interceptMessage[IllegalArgumentException](msg){
      sj.read[Either[String, String]](js.asInstanceOf[JSON])
    }        
  }

  test("Neither value works") {
    val js = "25"
    val msg = """Failed to read either side of Either
                |25
                |^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[Either[String, Boolean]](js.asInstanceOf[JSON])
    }        
  }
