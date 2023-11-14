package co.blocke

import org.openjdk.jmh.annotations._

object ScalaJackZ:
    import co.blocke.scalajack.* 

    import json2.*

    implicit val addrDecoder: JsonDecoder[Address] = ClassDecoder(
      Array("street", "city", "state", "postal_code"),
      Array(JsonDecoder[String], JsonDecoder[String], JsonDecoder[String], JsonDecoder[String])
    )
    implicit val friendDecoder: JsonDecoder[Friend] = ClassDecoder(
      Array("name", "age", "email"),
      Array(JsonDecoder[String], JsonDecoder[Int], JsonDecoder[String])
    )
    implicit val petDecoder: JsonDecoder[Pet] = ClassDecoder(
      Array("name", "species", "age"),
      Array(JsonDecoder[String], JsonDecoder[String], JsonDecoder[Int])
    )
    implicit val PersonDecoder: JsonDecoder[Person] = ClassDecoder(
      Array("name", "age", "address", "email", "phone_numbers", "is_employed"),
      Array(JsonDecoder[String], JsonDecoder[Int], JsonDecoder[Address], JsonDecoder[String], JsonDecoder[List[String]], JsonDecoder[Boolean])
    )
    implicit val RecordDecoder: JsonDecoder[Record] = ClassDecoder(
      Array("person", "hobbies", "friends", "pets"),
      Array(JsonDecoder[Person], JsonDecoder[List[String]], JsonDecoder[List[Friend]], JsonDecoder[List[Pet]])
    ) 

    // val jp = co.blocke.scalajack.json.JsonParser3(jsData)

    trait ScalaJackReadingBenchmark{
    @Benchmark
    // def readRecordScalaJack = { jp.reset(); jp.parse() }
    // def readRecordScalaJack = ScalaJack.read[Record](jsData)
    def readRecordScalaJack = JsonDecoder[Record].decodeJson(jsData)
    }

    trait ScalaJackWritingBenchmark { 
    @Benchmark
    def writeRecordScalaJack = ScalaJack.write(record)
    }