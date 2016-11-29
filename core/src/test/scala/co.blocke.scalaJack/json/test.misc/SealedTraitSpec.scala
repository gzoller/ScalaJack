package co.blocke.scalajack
package json.test.misc

import org.scalatest.{ BeforeAndAfterAll, FunSpec, GivenWhenThen }

import scala.reflect.api.{ Mirror, Universe }
import scala.reflect.runtime.{ currentMirror, universe }
import scala.reflect.runtime.universe.{ TypeTag, typeOf, Type }

// Unambiguous member names
sealed trait ContactPoint
case class EmailAddress(emailAddress: String) extends ContactPoint
case class PhoneNumber(phoneNumber: String) extends ContactPoint

// Ambiguous member names
sealed trait Vehicle
case class Truck(numberOfWheels: Int) extends Vehicle
case class Car(numberOfWheels: Int, color: String) extends Vehicle
case class Plane(numberOfEngines: Int) extends Vehicle

class SealedTraitSpec extends FunSpec with GivenWhenThen with BeforeAndAfterAll {

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
        sj.read[Vehicle]("""{"_hint":"co.blocke.scalajack.json.test.misc.Truck","numberOfWheels":4}""")
      }
    }

    it("Write - ambiguous") {
      assertResult("""{"_hint":"co.blocke.scalajack.json.test.misc.Car","numberOfWheels":3,"color":"Red"}""") {
        sj.render[Vehicle](Car(numberOfWheels = 3, color = "Red"))
      }
    }
  }

}
