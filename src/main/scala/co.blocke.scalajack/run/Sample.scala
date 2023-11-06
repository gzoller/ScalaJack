package co.blocke.scalajack.run

import neotype.*

// opaque type BigName = String

// case class Person(name: String, age: Int, isOk: List[Boolean], favColor: Colors, boss: BigName)

// trait Animal:
//   val name: String
//   val numLegs: Int
//   val friend: Option[Animal]

// trait Animal2:
//   val name: String
//   val numLegs: Int
//   val friend: Option[Animal2]

// case class Dog(name: String, numLegs: Int, carsChased: Int, friend: Option[Animal2]) extends Animal2

enum Colors:
  case Red, Blue, Green

// import scala.collection.immutable.*
// enum Vehicle:
//   case Car, Bus, Train

object WeekDay extends Enumeration {
  type WeekDay = Value
  val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
}
import WeekDay.*

// case class Simple(a: Int, b: Boolean, c: Option[Simple], z: Int = 5)

// case class Blah(msg: String, stuff: WeekDay)

// object Talk:
//   def say(s: String): String = s"Say $s!"

// case class M1(v: Map[Long, Int], v2: HashMap[Colors, Int], v3: Map[co.blocke.scala_reflection.TypedName, Int])

trait Miss[E] { val x: E }
case class Foom[X](x: X) extends Miss[X]

// case class Person[Y](name: String, age: Miss[Y], again: Option[Person[Y]])

case class Person(name: String, color: java.util.HashMap[Int, Boolean])

// type NonEmptyString = NonEmptyString.Type
// given NonEmptyString: Newtype[String] with
//   inline def validate(input: String): Boolean =
//     input.nonEmpty

// type MinorPerson = MinorPerson.Type
// given MinorPerson: Newtype[Person] with
//   inline def validate(input: Person): Boolean =
//     input.age < 18

// case class SampleNeo(name: NonEmptyString, label: String, unknown: MinorPerson)
