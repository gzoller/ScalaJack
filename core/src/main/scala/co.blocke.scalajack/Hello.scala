package co.blocke.scalajack

import org.json4s._
import json4s._
import scala.reflect.runtime.universe._

trait Address { val postalCode: String }
case class USAddress(
    street:     String,
    city:       String,
    state:      String,
    postalCode: String)
  extends Address
case class DefaultAddress(postalCode: String) extends Address

object Hello extends App {
  val sj = ScalaJack(Json4sFlavor())
    .parseOrElse(typeOf[Address] -> typeOf[DefaultAddress])

  val js4s =
    JObject(
      List("_hint" -> JString("unknown"), "postalCode" -> JString("12345"))
    )
  println(sj.read[Address](js4s))
}
