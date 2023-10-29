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

object WeekDay extends Enumeration {
  type WeekDay = Value
  val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
}
import WeekDay.*

case class Simple(a: Int, b: Boolean, c: Option[Simple], z: Int = 5)

case class Blah(msg: String, stuff: WeekDay)

object Talk:
  def say(s: String): String = s"Say $s!"

case class M1(v: Map[Long, Int], v2: HashMap[Colors, Int], v3: Map[co.blocke.scala_reflection.TypedName, Int])
