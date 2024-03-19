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

// import io.circe.{Codec,Decoder,Encoder,Json}
// import io.circe.generic.semiauto._
// case class Who(id: String, name:String, `type`:Who.Type)
// object Who:
//   implicit val codec: Codec[Who] = deriveCodec[Who]

//   sealed trait Type extends Product with Serializable
//   object Type :
//     val values: Set[Who.Type] = Set(Staff, Customer, Program)
//     implicit val encoder: Encoder[Type] = ???
//     implicit val decoder: Decoder[Type] = ???
//     case object Staff extends Type
//     case object Customer extends Type
//     case object Program extends Type

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
