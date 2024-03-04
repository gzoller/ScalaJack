package co.blocke

case class Person2(
  name: String,
  age: Int,
  address: Address2,
  email: String,
  phone_numbers: List[String],
  is_employed: Boolean
)

case class Address2(
  street: String,
  city: String,
  state: String,
  postal_code: String
)

case class Friend2(
  name: String,
  age: Int,
  email: String
)

case class Pet2(
  name: String,
  species: String,
  age: Int
)

case class Record2(
  person: Person2,
  hobbies: List[String],
  friends: List[Friend2],
  pets: List[Pet2]
)


val jsData2 = 
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