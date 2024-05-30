package co.blocke

import org.openjdk.jmh.annotations._

object PlayZ:
  import play.api.libs.json._
  import play.api.libs.json.Reads._
  import play.api.libs.functional.syntax._
  
  implicit val friendWrites: Writes[Friend2] = (
    (JsPath \ "name").write[String] and
    (JsPath \ "age").write[Int] and
    (JsPath \ "email").write[String]
  )(unlift((a: Friend2) => Option((a.name, a.age, a.email))))

  implicit val petWrites: Writes[Pet2] = (
    (JsPath \ "name").write[String] and
    (JsPath \ "species").write[String] and
    (JsPath \ "age").write[Int]
  )(unlift((a: Pet2) => Option((a.name, a.species, a.age))))

  implicit val addressWrites: Writes[Address2] = (
    (JsPath \ "street").write[String] and
    (JsPath \ "city").write[String] and
    (JsPath \ "state").write[String] and
    (JsPath \ "postal_code").write[String]
  )(unlift((a: Address2) => Option((a.street, a.city, a.state, a.postal_code))))

  implicit val personWrites: Writes[Person2] = (
    (JsPath \ "name").write[String] and
    (JsPath \ "age").write[Int] and
    (JsPath \ "address").write[Address2] and
    (JsPath \ "email").write[String] and
    (JsPath \ "phone_numbers").write[List[String]] and
    (JsPath \ "is_employed").write[Boolean]
  )(unlift((a: Person2) => Option((a.name, a.age, a.address, a.email, a.phone_numbers, a.is_employed))))

  implicit val recordWrites: Writes[Record2] = (
    (JsPath \ "person").write[Person2] and
    (JsPath \ "hobbies").write[List[String]] and
    (JsPath \ "friends").write[List[Friend2]] and
    (JsPath \ "pets").write[List[Pet2]]
  )(unlift((a: Record2) => Option((a.person, a.hobbies, a.friends, a.pets))))

  implicit val friendReads: play.api.libs.json.Reads[co.blocke.Friend2] = Json.reads[Friend2]
  implicit val petReads: play.api.libs.json.Reads[co.blocke.Pet2] = Json.reads[Pet2]
  implicit val addressReads: play.api.libs.json.Reads[co.blocke.Address2] = Json.reads[Address2]
  implicit val personReads: play.api.libs.json.Reads[co.blocke.Person2] = Json.reads[Person2]
  implicit val recordReads: play.api.libs.json.Reads[co.blocke.Record2] = Json.reads[Record2]

  trait PlayWritingBenchmark { 
    @Benchmark
    def writeRecordPlay = Json.toJson(record)
  }

  // val playJS = Json.toJson(record)
  trait PlayReadingBenchmark { 
    @Benchmark
    def readRecordPlay =  Json.fromJson[Record2](Json.parse(jsData2)) //Json.fromJson[Record](playJS)
  }
