package co.blocke.scalajack
package json
package classes

import co.blocke.scala_reflection.Ignore
import dotty.tools.repl.Command
import java.net.NoRouteToHostException

case class Person(name: String, @Change(name = "duration") age: Int)

class Parent(val phase: Int, var stuff: List[String]):
  private var _hidden: Boolean = false
  def hidden: Boolean = _hidden
  def hidden_=(h: Boolean) = _hidden = h

  private var _nope: Boolean = false // should not generate due to @Ignore
  @Ignore def nope: Boolean = _nope
  def nope_=(h: Boolean) = _nope = h

  var foo: String = "ok"
  @Ignore var noFoo: String = "not ok"

case class Child(name: String, age: Int, override val phase: Int) extends Parent(phase, Nil)

case class Params[X, Y](a: List[X], b: Option[Y])

sealed trait Command
case object Start extends Command
case object Stop extends Command

sealed trait Animal
@TypeHint(hintValue = "bowow")
case class Dog(name: String, numLegs: Int) extends Animal
@TypeHint(hintValue = "flipper")
case class Fish(species: String, freshwater: Boolean) extends Animal

sealed trait City
class Dallas(val pop: Int) extends City
@TypeHint(hintValue = "vice")
class Miami(val temp: Double) extends City

sealed trait Route
class CityRoute(val numStreets: Int) extends Route
// Testing indirection. In real-world scenario all your sealed trait's classes
// must be defined in one file. Implementation classes like CityRouteImpl could
// be in other files so the sealed trait's file doesn't grow huge.

case class TraitHolder(a: Command, b: Animal, c: City, d: Route)

sealed abstract class Command2
case object Start2 extends Command2
case object Stop2 extends Command2

sealed abstract class Animal2
@TypeHint(hintValue = "bowow")
case class Dog2(name: String, numLegs: Int) extends Animal2
@TypeHint(hintValue = "flipper")
case class Fish2(species: String, freshwater: Boolean) extends Animal2

sealed abstract class City2
class Dallas2(val pop: Int) extends City2
@TypeHint(hintValue = "vice")
class Miami2(val temp: Double) extends City2

sealed abstract class AThing[T]
class Thing1[T](val t: T) extends AThing[T]
class Thing2[T](val t: T, val s: String) extends AThing[T]

case class AbstractClassHolder(a: Command2, b: Animal2, c: City2)
case class AbstractClassHolder2[P](a: AThing[P])

case class Empl[T](id: String, data: T, boss: Empl[T], coworkers: List[Empl[T]])

object VehicleClass extends Enumeration {
  type VehicleClass = Value
  val Land, Air, Sea = Value
}
import VehicleClass.*

sealed trait Vehicle { val kind: VehicleClass }
case class Car(passengers: Int) extends Vehicle { val kind: Land.type = Land }

sealed trait Hobby[X, Y] { val thing1: X; val thing2: Y }
sealed trait Artist[W, Z] { val instrument: W; val effort: Z }
sealed trait PersonX[X, Y] { val who: X; val org: Y }

case class Sports[A, B](thing1: A, thing2: B) extends Hobby[A, B]
case class Painter[A, B](instrument: A, effort: B) extends Artist[A, B]
case class Employee[A, B, C, D](who: Artist[C, Hobby[D, A]], org: B) extends PersonX[Artist[C, Hobby[D, A]], B]
type ComplexPerson = PersonX[Artist[Int, Hobby[Double, Char]], Vehicle]

sealed trait Machine
case class Press(name: String, lbs: Int) extends Machine
case class Lift(name: String, lbs: Int, foo: Boolean) extends Machine
case class Drill(name: String, numBits: Int) extends Machine
case class Swing(name: String, lbs: Option[Int], isBig: Boolean) extends Machine
case class MachineHolder(m1: Machine, m2: Machine, m3: Machine, m4: Machine)

// These should always generate a type hint!
sealed trait Machine2
case class Press2(name: String, lbs: Option[Int]) extends Machine2
case class Lift2(name: String, lbs: Option[Int]) extends Machine2
case class Drill2(name: String, lbs: Option[Int], numBits: Int) extends Machine2
case class MachineHolder2(m1: Machine2, m2: Machine2, m3: Machine2)

// Unique field key of "" and complex (nested) trait
sealed trait Level0
case class L0A(x: Int, name: Option[String], y: Boolean) extends Level0
case class L0B(name: Option[String], id: String, blather: Option[String]) extends Level0
case class L0C(id: Option[Int], extra: List[Int]) extends Level0
sealed trait Level1
case class L1R(blather: String, name: Option[String], l0: Level0) extends Level1
case class L1S(id: Long, nombre: String) extends Level1
case class L1Q(name: String, age: Int, l0: Level0) extends Level1
case class L1X(name: String, age: Int, l0: Level0) extends Level1
case class ComplexHolder(c1: Level1, c2: Level1, c3: Level1, c4: Level1)
