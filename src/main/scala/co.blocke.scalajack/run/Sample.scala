package co.blocke.scalajack.run

opaque type BigName = String

case class Person(name: String, age: Int, isOk: List[Boolean], favColor: Colors, boss: BigName)

trait Animal:
  val name: String
  val numLegs: Int
  val friend: Option[Animal]

trait Animal2:
  val name: String
  val numLegs: Int
  val friend: Option[Animal2]

case class Dog(name: String, numLegs: Int, carsChased: Int, friend: Option[Animal2]) extends Animal2

enum Colors:
  case Red, Blue, Green

import scala.collection.immutable.*
enum Vehicle:
  case Car, Bus, Train

case class Simple(a: Int, b: Boolean, c: Option[String], z: Int = 5)

case class Blah(msg: String, stuff: Simple)

object Talk:
  def say(s: String): String = s"Say $s!"
