package co.blocke.scalajack
package json.misc

import org.scalatest.{ BeforeAndAfterAll, FunSpec, GivenWhenThen }

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

class SealedTraits extends FunSpec with GivenWhenThen with BeforeAndAfterAll {

  val sj = ScalaJack()

  describe("-----------------------------\n:  Sealed Trait Tests  :\n-----------------------------") {
    it("Read - unambiguous") {
      assertResult(EmailAddress("foo@bar.com")) {
        sj.read[ContactPoint]("""{"emailAddress":"foo@bar.com"}""")
      }
    }
    it("Write - unambiguous") {
      assertResult("""{"phoneNumber":"12223334444"}""") {
        sj.render[ContactPoint](PhoneNumber("12223334444"))
      }
    }
    it("Read - ambiguous") {
      assertResult(Truck(numberOfWheels = 4)) {
        sj.read[Vehicle]("""{"_hint":"co.blocke.scalajack.json.misc.Truck","numberOfWheels":4}""")
      }
    }
    it("Write - ambiguous") {
      assertResult("""{"_hint":"co.blocke.scalajack.json.misc.Car","numberOfWheels":3,"color":"Red"}""") {
        sj.render[Vehicle](Car(numberOfWheels = 3, color = "Red"))
      }
    }
    it("Case object implementation") {
      val flavors: List[Flavor] = List(Bourbon, Vanilla, Chocolate)
      val js = sj.render(flavors)
      assertResult("""["Bourbon","Vanilla","Chocolate"]""") { js }
      assertResult(flavors) { sj.read[List[Flavor]](js) }
    }
    it("Type hints with modification") {
      val sj2 = ScalaJack().withHints((typeOf[Stay] -> "stay_kind"))
      val s: Stay = VillaStay("Hacienda")
      val js = sj2.render(s)
      assertResult("""{"stay_kind":"co.blocke.scalajack.json.misc.VillaStay","name":"Hacienda"}""") { js }
      assertResult(s) { sj2.read[Stay](js) }
    }
  }

}
