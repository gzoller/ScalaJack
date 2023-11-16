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

case class Foo(name: String, maybe: Option[Int], age: Int, expected: String = "nada", gotit: Option[Int] = Some(5))

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
