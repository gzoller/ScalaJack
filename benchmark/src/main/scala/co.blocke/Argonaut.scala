package co.blocke

import org.openjdk.jmh.annotations._


object ArgonautZ:
  import argonaut._, Argonaut._

  implicit val CodecPet: CodecJson[Pet2] = 
    casecodec3(Pet2.apply, (a: Pet2) => Option((a.name, a.species, a.age)))("name","species","age")

  implicit val CodecFriend: CodecJson[Friend2] = 
    casecodec3(Friend2.apply, (a: Friend2) => Option((a.name, a.age, a.email)))("name","age","email")

  implicit val CodecAddress: CodecJson[Address2] = 
    casecodec4(Address2.apply, (a: Address2) => Option((a.street, a.city, a.state, a.postal_code)))("street","city","state","postal_code")

  implicit val CodecPerson: CodecJson[Person2] = 
    casecodec6(Person2.apply, (a: Person2) => Option((a.name, a.age, a.address, a.email, a.phone_numbers, a.is_employed)))("name", "age","address","email","phone_numbers","is_employed")

  implicit val CodecRecord: CodecJson[Record2] =
    casecodec4(Record2.apply, (a: Record2) => Option((a.person, a.hobbies, a.friends, a.pets)))("person", "hobbies", "friends", "pets")


  trait ArgonautReadingBenchmark {
    @Benchmark
    def readRecordArgonaut = Parse.decodeEither[Record2](jsData2)
  }

  trait ArgonautWritingBenchmark {
    @Benchmark
    def writeRecordArgonaut = record.asJson
  }
