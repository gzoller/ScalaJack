package co.blocke.scalajack.run

opaque type BigName = String

case class Person(name: String, age: Int, isOk: List[Boolean], favColor: Colors, boss: BigName)

trait Animal:
  val name: String
  val numLegs: Int
  val friend: Option[Animal]

case class Dog(name: String, numLegs: Int, carsChased: Int, friend: Option[Animal]) extends Animal

enum Colors:
  case Red, Blue, Green
