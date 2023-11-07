package co.blocke.scalajack.run

import neotype.*

// Enumeration sealed trait
sealed trait card extends Enumeration
case object CLUB extends card
case object HEART extends card
case object DIAMOND extends card
case object SPADE extends card

sealed trait msg[X]
case class Command[T](item: T) extends msg[T]
case class Query[T](item: T) extends msg[T]

enum Colors:
  case Red, Blue, Green

trait Miss[E] { val x: E }
case class Foom[X](x: X) extends Miss[X]

// case class Person[Y](name: String, age: Miss[Y], again: Option[Person[Y]])

case class Person[T](name: String, card: card, msg: msg[T], miss: Miss[T])

// type NonEmptyString = NonEmptyString.Type
// given NonEmptyString: Newtype[String] with
//   inline def validate(input: String): Boolean =
//     input.nonEmpty

// type MinorPerson = MinorPerson.Type
// given MinorPerson: Newtype[Person] with
//   inline def validate(input: Person): Boolean =
//     input.age < 18

// case class SampleNeo(name: NonEmptyString, label: String, unknown: MinorPerson)
