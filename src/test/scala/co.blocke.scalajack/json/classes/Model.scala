package co.blocke.scalajack
package json
package classes

import co.blocke.scala_reflection.Ignore
import dotty.tools.repl.Command

case class Person(name: String, age: Int)

class Parent(val phase: Int):
  private var _hidden: Boolean = false
  def hidden: Boolean = _hidden
  def hidden_=(h: Boolean) = _hidden = h

  private var _nope: Boolean = false // should not generate due to @Ignore
  @Ignore def nope: Boolean = _nope
  def nope_=(h: Boolean) = _nope = h

  var foo: String = "ok"
  @Ignore var noFoo: String = "not ok"

case class Child(name: String, age: Int, override val phase: Int) extends Parent(phase)

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

case class TraitHolder(a: Command, b: Animal, c: City)

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

case class AbstractClassHolder(a: Command2, b: Animal2, c: City2)

case class Empl[T](id: String, data: T, boss: Empl[T], coworkers: List[Empl[T]])
