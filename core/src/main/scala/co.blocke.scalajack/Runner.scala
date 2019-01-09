package co.blocke.scalajack

import json._

case class Person(name: String, age: Int, other: Option[Long])
trait Pet {
  val numLegs: Int
  val name: String
}
case class Dog(name: String, numLegs: Int, weight: Double)

sealed trait Currency { def name: String }
case object EUR extends Currency { val name = "EUR" }
case object USD extends Currency { val name = "USD" }

sealed trait ContactPoint
case class EmailAddress(emailAddress: String) extends ContactPoint
case class PhoneNumber(phoneNumber: String) extends ContactPoint

sealed trait Vehicle
case class Truck(numberOfWheels: Int) extends Vehicle
case class Car(numberOfWheels: Int, color: String) extends Vehicle
case class Plane(numberOfEngines: Int) extends Vehicle

case class VCLong(vc: Long) extends AnyVal

object Runner extends App {

  val sj = ScalaJack().forType[List[List[String]]]

  println(sj.fastRead("""[["a","b","c"],["a","b","c"]]"""))

  println(sj.read[Map[String, Int]]("""{"a":1,"b":2}"""))

  println(sj.read[Map[List[Int], Boolean]]("""{"[1,2,3]":true,"[4,5,6]":false}"""))

  println(sj.read[Person]("""{"name":"Greg","age":52}"""))

  println(sj.read[Pet]("""{"name":"George","numLegs":4,"_hint":"co.blocke.scalajack.Dog","weight": 12.34}"""))

  println(sj.read[Map[Any, Int]]("""{"[1,2,3]":25}"""))

  println(sj.read[Any]("""{"name":"George","numLegs":4,"_hint":"co.blocke.scalajack.Dog","weight": 12.34}"""))

  //  println(sj.read[Currency]("\"USD\""))

  println(sj.read[ContactPoint]("""{"emailAddress":"foo@bar.com"}"""))
  println(sj.read[Vehicle]("""{"_hint":"co.blocke.scalajack.Truck","numberOfWheels":4}"""))

  println(sj.read[VCLong]("100"))

  println("WRITE: " + sj.write(Map("x" -> "foo", "y" -> """bar "none"""")))

  //  val t = JsonTokenizer()
  //  val tok = t.tokenize("""{"name":"Greg\"Zoller","age":52}""")
  //  println(tok.toList)
}
