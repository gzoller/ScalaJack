package co.blocke.scalajack
package run

case class Person(
    name: String,
    age: Int,
    address: Address,
    email: String,
    phone_numbers: List[String],
    is_employed: Boolean
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
case class Foo(a: Empl, b: List[Empl])
case class Blah(a: String, b: String | Option[Int])
/*
case class ArrayHolder[T](a: Array[T])

// case class Foo(name: String, maybe: Option[Int], age: Int, expected: String = "nada", gotit: Option[Int] = Some(5))
case class Foo(name: String, a: Animal, other: Option[Foo], color: Color, expected: String = "nada")
// case class Foo(name: String, age: Int, expected: String = "nada")

enum Color:
  case Red, Blue, Green

sealed trait Animal
@TypeHint(hintValue = "bow-wow")
case class Dog(name: String, numLegs: Int) extends Animal
@TypeHint(hintValue = "flippy")
case class Fish(name: String, isFreshwater: Option[Boolean]) extends Animal
 */

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

val record = Record(
  Person("John Doe", 30, Address("123 Main Street", "Anytown", "CA", "12345"), "john.doe@example.com", List("555-555-5555", "555-123-4567"), true),
  List("reading", "swimming", "traveling"),
  List(Friend("Jane Smith", 28, "jane.smith@example.com"), Friend("Bob Johnson", 32, "bob.johnson@example.com")),
  List(Pet("Fido", "Dog", 5), Pet("Whiskers", "Cat", 3))
)
