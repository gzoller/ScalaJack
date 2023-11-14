package co.blocke

import org.openjdk.jmh.annotations._

object PlayZ:
  import play.api.libs.json._
  import play.api.libs.json.Reads._
  import play.api.libs.functional.syntax._
  
  implicit val friendWrites: Writes[Friend] = (
    (JsPath \ "name").write[String] and
    (JsPath \ "age").write[Int] and
    (JsPath \ "email").write[String]
  )(unlift((a: Friend) => Option((a.name, a.age, a.email))))

  implicit val petWrites: Writes[Pet] = (
    (JsPath \ "name").write[String] and
    (JsPath \ "species").write[String] and
    (JsPath \ "age").write[Int]
  )(unlift((a: Pet) => Option((a.name, a.species, a.age))))

  implicit val addressWrites: Writes[Address] = (
    (JsPath \ "street").write[String] and
    (JsPath \ "city").write[String] and
    (JsPath \ "state").write[String] and
    (JsPath \ "postal_code").write[String]
  )(unlift((a: Address) => Option((a.street, a.city, a.state, a.postal_code))))

  implicit val personWrites: Writes[Person] = (
    (JsPath \ "namet").write[String] and
    (JsPath \ "age").write[Int] and
    (JsPath \ "address").write[Address] and
    (JsPath \ "email").write[String] and
    (JsPath \ "phone_numbers").write[List[String]] and
    (JsPath \ "is_employed").write[Boolean]
  )(unlift((a: Person) => Option((a.name, a.age, a.address, a.email, a.phone_numbers, a.is_employed))))

  implicit val recordWrites: Writes[Record] = (
    (JsPath \ "person").write[Person] and
    (JsPath \ "hobbies").write[List[String]] and
    (JsPath \ "friends").write[List[Friend]] and
    (JsPath \ "pets").write[List[Pet]]
  )(unlift((a: Record) => Option((a.person, a.hobbies, a.friends, a.pets))))

  implicit val friendReads: play.api.libs.json.Reads[co.blocke.Friend] = Json.reads[Friend]
  implicit val petReads: play.api.libs.json.Reads[co.blocke.Pet] = Json.reads[Pet]
  implicit val addressReads: play.api.libs.json.Reads[co.blocke.Address] = Json.reads[Address]
  implicit val personReads: play.api.libs.json.Reads[co.blocke.Person] = Json.reads[Person]
  implicit val recordReads: play.api.libs.json.Reads[co.blocke.Record] = Json.reads[Record]

  trait PlayWritingBenchmark { 
    @Benchmark
    def writeRecordPlay = Json.toJson(record)
  }

  // val playJS = Json.toJson(record)
  trait PlayReadingBenchmark { 
    @Benchmark
    def readRecordPlay =  Json.fromJson[Record](Json.parse(jsData)) //Json.fromJson[Record](playJS)
  }
