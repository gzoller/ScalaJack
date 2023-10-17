package co.blocke.scalajack
package json.structures

import co.blocke.scala_reflection._
import TestUtil._
import munit._
import munit.internal.console
import co.blocke.scalajack.json.JSON

// Unambiguous member names
sealed trait ContactPoint
case class EmailAddress(emailAddress: String) extends ContactPoint
case class PhoneNumber(phoneNumber: String) extends ContactPoint

// Ambiguous member names
sealed trait Vehicle
case class Truck(numberOfWheels: Int) extends Vehicle
case class Car(numberOfWheels: Int, color: String) extends Vehicle
case class Plane(numberOfEngines: Int) extends Vehicle

// Case object implementation
sealed trait Flavor
case object Vanilla extends Flavor
case object Chocolate extends Flavor
case object Bourbon extends Flavor

sealed trait Stay
case class VillaStay(name: String) extends Stay
case class RanchStay(name: String) extends Stay

case class NotSealed()


class SealedTraits() extends FunSuite:

  val sj = co.blocke.scalajack.ScalaJack()
  
  test("Read - unambiguous") {
    describe("------------------------\n:  Sealed Trait Tests  :\n------------------------",Console.BLUE)

    assert(EmailAddress("foo@bar.com") == sj.read[ContactPoint]("""{"emailAddress":"foo@bar.com"}""".asInstanceOf[JSON]))
  }

  test("Write - unambiguous") {
    assertEquals("""{"phoneNumber":"12223334444"}""".asInstanceOf[JSON], sj.render[ContactPoint](PhoneNumber("12223334444")))
  }

  test("Read - ambiguous") {
    val msg =
      """Multiple sub-classes of co.blocke.scalajack.json.structures.Stay match field names Set(name)
        |{"name":"Wilderness"}
        |--------------------^""".stripMargin
    val js = """{"name":"Wilderness"}""".asInstanceOf[JSON]
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[Stay](js)
    }
  }

  test("Write - ambiguous") {
    assertEquals(
      """{"name":"Wilderness"}""".asInstanceOf[JSON], sj.render[Stay](VillaStay("Wilderness")))
  }

  test("Case object implementation") {
    val flavors: List[Flavor] = List(Bourbon, Vanilla, Chocolate)
    val js = sj.render(flavors)
    assertEquals("""["Bourbon","Vanilla","Chocolate"]""".asInstanceOf[JSON],js)
    assertEquals(flavors, sj.read[List[Flavor]](js) )
  }

  // No more hints on sealed traits!!
  // test("Type hints with modification") {
  //   val sj2 = co.blocke.scalajack.ScalaJack().withHints((RType.of[Stay] -> "stay_kind"))
  //   val s: Stay = VillaStay("Hacienda")
  //   val js = sj2.render(s)
  //   assertEquals(
  //     """{"stay_kind":"co.blocke.scalajack.json.misc.VillaStay","name":"Hacienda"}""".asInstanceOf[JSON],js)
  //   assertEquals(s, sj2.read[Stay](js) )
  // }

  test("Handles null") {
    val js = """null""".asInstanceOf[JSON]
    val inst = sj.read[ContactPoint](js)
    assertEquals(inst, null)
    assert( sj.render[ContactPoint](inst) == js)
  }

  test("Handle not a sealed trait") {
    val js = """{"d":3,"color":"Red"}""".asInstanceOf[JSON]
    val msg =
      """No sub-classes of co.blocke.scalajack.json.structures.ContactPoint match field names Set(d, color)
      |{"d":3,"color":"Red"}
      |--------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[ContactPoint](js)
    }
  }
  
  test("Resolved ambiguous trait") {
    val inst = Car(4,"Red")
    val js = sj.render(inst)
    assertEquals("""{"numberOfWheels":4,"color":"Red"}""".asInstanceOf[JSON], js)
    assert(inst == sj.read[Vehicle](js))
  }