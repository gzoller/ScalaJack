package co.blocke.scalajack

// $COVERAGE-OFF$This file is for debugging only!

import util._
import model._
import scala.util.Try

trait Address { val postalCode: String }
case class USAddress(street: String, city: String, state: String, postalCode: String) extends Address
trait Demographic { val address: Address }
object Food extends Enumeration {
  val Seeds, Meat, Pellets, Veggies = Value
}
trait Pet {
  val name: String
  val food: Food.Value
}
case class SamplePet(m: Map[Pet, Pet])

object Runner extends App {

  //  val sj = ScalaJack()

  val t1 = Try {
    val strMatchHintMod = StringMatchHintModifier(Map("US" -> typeOf[USAddress]))
    val sj = ScalaJack().withHintModifiers((typeOf[Address], strMatchHintMod))
    val js = """{"_hint":"co.blocke.scalajack.json.test.custom.USDemographic","age":50,"address":{"_hint":"Bogus","street":"123 Main","city":"New York","state":"NY","postalCode":"39822"}}"""
    sj.read[Demographic](js)
  }

  //----------------

  val t2 = Try {
    val js2 = """{"m":{"{\"_hint\":\"co.blocke.scalajack.json.test.mapkeys.Bogus\",\"name\":\"Flipper\",\"food\":\"Veggies\",\"waterTemp\":74.33}":{"_hint":"co.blocke.scalajack.json.test.mapkeys.DogPet","name":"Fido","food":"Meat","numLegs":3}}}"""
    ScalaJack().read[SamplePet](js2)
  }
}
// $COVERAGE-ON$

