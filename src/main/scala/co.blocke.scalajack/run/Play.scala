package co.blocke.scalajack
package run

import co.blocke.scala_reflection.*
import scala.jdk.CollectionConverters.*

object RunMe extends App:

  given json.JsonConfig = json
    .JsonConfig()
    .copy(noneAsNull = true)
    .copy(writeNonConstructorFields = true)
  // .copy(enumsAsIds = '*')

  try

    import json.*

    val js = """[{
        "street": "123 Main Street",
        "city": "Anytown",
        "state": "CA",
        "postal_code": "12345"
      }]"""
    println(ScalaJack.read[Record](jsData))

  /* StringMatrix experiments....
   */

  /*
    val matrix = new StringMatrix(List("foo", "bar", "baz").toArray, Array(("boom", 0)))
    val r = JsonReader("boom\" asdf ")
    var i: Int = 0
    var bs: Long = matrix.initial
    var c: Int = -1
    while { c = r.readEscapedString(); c != END_OF_STRING } do {
      bs = matrix.update(bs, i, c)
      i += 1
    }
    bs = matrix.exact(bs, i)
    println("HERE: " + matrix.first(bs))
   */

  // val numList = """[["1","2,3"],["4","5"]]"""
  // val dec = JsonDecoder[List[List[String]]]
  // println(dec.decodeJson(numList))

  /*
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

    val addr = """{"street":"319 Hampton Ct","city":"Coppell","state":"TX","postal_code":"75019"}"""
    val dec2 = JsonDecoder[Address]
    println(JsonDecoder[Record].decodeJson(jsData))
   */

  /*
    import parser.*
    import json.*

    // val inst = SeqInstruction[Int, List[Int]](IntInstruction())
    // val inst2 = SeqInstruction[List[Int], Set[Seq[Int]]](inst)
    val js = """{"hey":"wowowow","age":57,"other":[1,2,3]}"""
    //          0123456789012345678901234567890123456789012234567890

    println(ScalaJack.read[Record](jsData))

    val jp = json.JsonParser3(jsData)
    println(">>> " + jp.parse())

    // val p = JsonParser2(js, JsonConfig())
    // println("Result: " + p.parse(inst2))
   */

  catch {
    case t: Throwable =>
      println(s"BOOM ($t): " + t.getMessage)
      t.printStackTrace
  }
