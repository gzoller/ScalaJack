package co.blocke.scalajack
package run

import neotype.*

// Enumeration sealed trait
sealed trait card extends Enumeration
case object CLUB extends card
case object HEART extends card
case object DIAMOND extends card
case object SPADE extends card

sealed abstract class msg[X]
@TypeHint
case class Command[T](item: T) extends msg[T]
case class Query[T](item: T) extends msg[T]

class Wrapper(val underlying: Int) extends AnyVal

enum Colors:
  case Red, Blue, Green

trait Miss[E] { val x: E }
case class Foom[X](x: X) extends Miss[X]

// case class Person[Y](name: String, age: Miss[Y], again: Option[Person[Y]])

case class Person[T](val name: String, val card: card, val msg: msg[T], meh: Wrapper):
  var thingy: String = "wow"

  private var c: Int = 5
  def count: Int = c
  def count_=(x: Int) = c = x

// type NonEmptyString = NonEmptyString.Type
// given NonEmptyString: Newtype[String] with
//   inline def validate(input: String): Boolean =
//     input.nonEmpty

// type MinorPerson = MinorPerson.Type
// given MinorPerson: Newtype[Person] with
//   inline def validate(input: Person): Boolean =
//     input.age < 18

// case class SampleNeo(name: NonEmptyString, label: String, unknown: MinorPerson)
