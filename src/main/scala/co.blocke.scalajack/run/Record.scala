package co.blocke.scalajack
package json
package run

import neotype.*
import co.blocke.scalajack.json.schema.*

@additionalProperties(value = "true")
@id(value = "abc123")
@title(value = "The Greatest")
@description(value = "nothing special")
case class Person(
    name: String = "Greg",
    age: Int = 57,
    address: Address = Address("123 Main", "New York", "NY", "12345"),
    email: Option[String] = Some("wow.gmail.com"),
    phone_numbers: List[String] = List("a", "b"),
    is_employed: Boolean = true
)

case class Address(
    street: String,
    city: String,
    state: String,
    postal_code: String
)

case class Friend(
    name: String,
    age: Int,
    email: String
)

case class Pet(
    name: String,
    species: String,
    age: Int
)

case class Record(
    person: Person,
    hobbies: List[String],
    friends: List[Friend],
    pets: List[Pet]
)

case class Empl(name: String, age: Int)
// case class Foo(a: Empl, b: List[Empl])
case class Blah(a: String, b: String | Option[Int])
/*
case class ArrayHolder[T](a: Array[T])

// case class Foo(name: String, maybe: Option[Int], age: Int, expected: String = "nada", gotit: Option[Int] = Some(5))
case class Foo(name: String, a: Animal, other: Option[Foo], color: Color, expected: String = "nada")
// case class Foo(name: String, age: Int, expected: String = "nada")

enum Color:
  case Red, Blue, Green
 */

case class Foo(name: String, a: Animal, x: (String, Seq[Boolean]))
sealed abstract class Animal
@TypeHint(hintValue = "bow-wow")
case class Dog(name: String, numLegs: Int) extends Animal
@TypeHint(hintValue = "flippy")
case class Fish(name: String, @Change(name = "fresh") isFreshwater: Option[Boolean]) extends Animal

type NonEmptyString = NonEmptyString.Type
given NonEmptyString: Newtype[String] with
  inline def validate(input: String): Boolean =
    input.nonEmpty

type NonEmptyList = NonEmptyList.Type
given NonEmptyList: Newtype[List[Int]] with
  inline def validate(input: List[Int]): Boolean =
    input.nonEmpty

type NonZeroInt = NonZeroInt.Type
object NonZeroInt extends Newtype[Int]:
  override inline def validate(input: Int): Boolean =
    input != 0

type XList = XList.Type
given XList: Newtype[List[String]] with
  inline def validate(input: List[String]): Boolean =
    input(0) == "x"

case class Validated(num: NonZeroInt, name: NonEmptyString, xspot: XList, nada: List[EmptyString])

case class Tag[X](a: X)
given [A, B](using newType: Newtype.WithType[A, B], tag: Tag[A]): Tag[B] =
  newType.unsafeWrapF(tag)

type EmptyString = EmptyString.Type
given EmptyString: Newtype[String] with
  inline def validate(input: String): Boolean =
    input.isEmpty

case class Person2(age: XList)

case class Foom(a: schema.Schema)

case class Group(t: (Int, String, Boolean))

sealed trait Candy:
  val isSweet: Boolean
case class MMs(isSweet: Boolean) extends Candy
case class Musk(isSweet: Boolean) extends Candy
case class Veggies(yuks: String)

type Food = Candy | Veggies

val jsData =
  """{
    "person": {
      "name": "John Doe",
      "age": 30,
      "address": {
        "street": "123 Main Street",
        "city": "Anytown",
        "state": "CA",
        "postal_code": "12345"
      },
      "email": "john.doe@example.com",
      "phone_numbers": [
        "555-555-5555",
        "555-123-4567"
      ],
      "is_employed": true
    },
    "hobbies": [
      "reading",
      "swimming",
      "traveling"
    ],
    "friends": [
      {
        "name": "Jane Smith",
        "age": 28,
        "email": "jane.smith@example.com"
      },
      {
        "name": "Bob Johnson",
        "age": 32,
        "email": "bob.johnson@example.com"
      }
    ],
    "pets": [
      {
        "name": "Fido",
        "species": "Dog",
        "age": 5
      },
      {
        "name": "Whiskers",
        "species": "Cat",
        "age": 3
      }
    ]
  }"""

case class Yippy(a: (Int, List[String], Boolean) = (5, List("a", "b"), false), b: Boolean)
sealed trait Flavor
case object Vanilla extends Flavor
case object ChocolateX extends Flavor
case object Bourbon extends Flavor
case class FlavorHolder(f: Flavor)

enum Colors:
  case Red, Green, Blue

object WeekDay extends Enumeration {
  type WeekDay = Value
  val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
}
import WeekDay.*

val record = Record(
  Person("John Doe", 30, Address("123 Main Street", "Anytown", "CA", "12345"), Some("john.doe@example.com"), List("555-555-5555", "555-123-4567"), true),
  List("reading", "swimming", "traveling"),
  List(Friend("Jane Smith", 28, "jane.smith@example.com"), Friend("Bob Johnson", 32, "bob.johnson@example.com")),
  List(Pet("Fido", "Dog", 5), Pet("Whiskers", "Cat", 3))
)
