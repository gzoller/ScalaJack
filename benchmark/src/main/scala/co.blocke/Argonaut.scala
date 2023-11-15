package co.blocke

import org.openjdk.jmh.annotations._


object ArgonautZ:
  import argonaut._, Argonaut._

  implicit val CodecPet: CodecJson[Pet] = 
    casecodec3(Pet.apply, (a: Pet) => Option((a.name, a.species, a.age)))("name","species","age")

  implicit val CodecFriend: CodecJson[Friend] = 
    casecodec3(Friend.apply, (a: Friend) => Option((a.name, a.age, a.email)))("name","age","email")

  implicit val CodecAddress: CodecJson[Address] = 
    casecodec4(Address.apply, (a: Address) => Option((a.street, a.city, a.state, a.postal_code)))("street","city","state","postal_code")

  implicit val CodecPerson: CodecJson[Person] = 
    casecodec6(Person.apply, (a: Person) => Option((a.name, a.age, a.address, a.email, a.phone_numbers, a.is_employed)))("name", "age","address","email","phone_numbers","is_employed")

  implicit val CodecRecord: CodecJson[Record] =
    casecodec4(Record.apply, (a: Record) => Option((a.person, a.hobbies, a.friends, a.pets)))("person", "hobbies", "friends", "pets")


  trait ArgonautReadingBenchmark {
    @Benchmark
    def readRecordArgonaut = Parse.decodeEither[Record](jsData)
  }

  trait ArgonautWritingBenchmark {
    @Benchmark
    def writeRecordArgonaut = record.asJson
  }
