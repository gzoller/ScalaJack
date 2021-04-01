package co.blocke.scalajack
package mongo

import co.blocke.scalajack.json.JsonFlavor
import co.blocke.scalajack.model.JackFlavor
import TestUtil._
import munit._
import munit.internal.console

import yaml._
import json._
import json4s._
import delimited._
import Converters._
import mongo.Converters._
import org.json4s._
import org.bson._
import scala.jdk.CollectionConverters._

trait HumanM
case class PersonM(@DBKey name: String, age: Int) extends HumanM
case class TypeyM[T](thing: T) {
  type foom = T
}

class ConverterSpec extends FunSuite:

  implicit val sj: JsonFlavor              = ScalaJack()
  implicit val sjY: JackFlavor[YAML]       = ScalaJack(YamlFlavor())
  implicit val sjV: JackFlavor[JValue]     = ScalaJack(Json4sFlavor())
  implicit val sjD: JackFlavor[DELIMITED]  = ScalaJack(DelimitedFlavor())
  implicit val sjB: JackFlavor[BsonValue]  = ScalaJack(MongoFlavor())

  val simple: PersonM         = PersonM("Fred", 34)
  val complex: TypeyM[HumanM] = TypeyM[HumanM](PersonM("Fred", 34))
  val simpleJson             = """{"name":"Fred","age":34}""".asInstanceOf[JSON]
  val complexJson            = """{"foom":"co.blocke.scalajack.mongo.PersonM","thing":{"name":"Fred","age":34}}""".asInstanceOf[JSON]
  val simpleJson4s           = JObject(List(("name", JString("Fred")), ("age", JInt(34))))
  val complexJson4s = JObject(
    List(
      ("foom", JString("co.blocke.scalajack.mongo.PersonM")),
      ("thing", JObject(List(("name", JString("Fred")), ("age", JInt(34)))))
    ))
  val simpleYaml      = """name: Fred
                     |age: 34
                     |""".stripMargin.asInstanceOf[YAML]
  val complexYaml     = """foom: co.blocke.scalajack.mongo.PersonM
                      |thing:
                      |  name: Fred
                      |  age: 34
                      |""".stripMargin.asInstanceOf[YAML]
  val simpleDelimited = "Fred,34".asInstanceOf[DELIMITED]
  val simpleMongo = new BsonDocument(
    List(
      new BsonElement("_id", new BsonString("Fred")),
      new BsonElement("age", new BsonInt32(34))
    ).asJava
  )
  val complexMongo = new BsonDocument(
    List(
      new BsonElement("foom", new BsonString("co.blocke.scalajack.mongo.PersonM")),
      new BsonElement("thing",
        new BsonDocument(
          List(
            new BsonElement("_id", new BsonString("Fred")),
            new BsonElement("age", new BsonInt32(34))
          ).asJava
        ))
    ).asJava
  )

  test("mapMongo") {
    describe(
      "-------------------------------------\n:  Converter Mapping Tests (Mongo)  :\n-------------------------------------", Console.BLUE
    )
    assertEquals(simpleMongo.mapMongo[PersonM](_.copy(age = 45)), new BsonDocument(
      List(
        new BsonElement("_id", new BsonString("Fred")),
        new BsonElement("age", new BsonInt32(45))
      ).asJava
    ))
  }

  test("mapMongoTo") {
    assertEquals(simpleMongo.mapMongoTo[PersonM, YAML](sjY)(_.copy(age = 45)), """name: Fred
                                                                                  |age: 45
                                                                                  |""".stripMargin.asInstanceOf[YAML])
  }

  test("toMongo") {
    describe(
      "-----------------------------------------\n:  Convenience \"to/from\" Tests (Mongo)  :\n-----------------------------------------", Console.BLUE
    )
    assertEquals(toMongo[PersonM](simple), simpleMongo)
    assertEquals(toMongo[TypeyM[HumanM]](complex), complexMongo)
  }

  test("fromMongo") {
    assertEquals(simpleMongo.fromMongo[PersonM], simple)
    assertEquals(complexMongo.fromMongo[TypeyM[HumanM]], complex)
  }

  test("mongoToJson") {
    describe(
      "------------------------------\n:  Converters Tests (Mongo)  :\n------------------------------", Console.BLUE
    )
    assertEquals(simpleMongo.mongoToJson[PersonM], simpleJson)
    assertEquals(complexMongo.mongoToJson[TypeyM[HumanM]], complexJson)
  }

  test("mongoToJson4s") {
    assertEquals(simpleMongo.mongoToJson4s[PersonM], simpleJson4s)
    assertEquals(complexMongo.mongoToJson4s[TypeyM[HumanM]], complexJson4s)
  }

  test("mongoToYaml") {
    assertEquals(simpleMongo.mongoToYaml[PersonM], simpleYaml)
    assertEquals(complexMongo.mongoToYaml[TypeyM[HumanM]], complexYaml)
  }

  test("jsonToMongo") {
    assertEquals(simpleJson.jsonToMongo[PersonM], simpleMongo)
    assertEquals(complexJson.jsonToMongo[TypeyM[HumanM]], complexMongo)
  }

  test("json4sToMongo") {
    assertEquals(simpleJson4s.json4sToMongo[PersonM], simpleMongo)
    assertEquals(complexJson4s.json4sToMongo[TypeyM[HumanM]], complexMongo)
  }

  test("yamlToMongo") {
    assertEquals(simpleJson4s.json4sToMongo[PersonM], simpleMongo)
    assertEquals(complexJson4s.json4sToMongo[TypeyM[HumanM]], complexMongo)
  }