package co.blocke.scalajack

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
import org.json4s._

trait Human
case class Person(name: String, age: Int) extends Human
case class Typey[T](thing: T) {
  type foom = T
}

class ConverterSpec extends FunSuite:

  implicit val sj: JsonFlavor              = ScalaJack()
  implicit val sjY: JackFlavor[YAML]       = ScalaJack(YamlFlavor())
  implicit val sjV: JackFlavor[JValue]     = ScalaJack(Json4sFlavor())
  implicit val sjD: JackFlavor[DELIMITED]  = ScalaJack(DelimitedFlavor())

  val simple: Person        = Person("Fred", 34)
  val complex: Typey[Human] = Typey[Human](Person("Fred", 34))
  val simpleJson            = """{"name":"Fred","age":34}""".asInstanceOf[JSON]
  val complexJson           = """{"foom":"co.blocke.scalajack.Person","thing":{"name":"Fred","age":34}}""".asInstanceOf[JSON]
  val simpleJson4s          = JObject(List(("name", JString("Fred")), ("age", JInt(34))))
  val complexJson4s = JObject(
    List(
      ("foom", JString("co.blocke.scalajack.Person")),
      ("thing", JObject(List(("name", JString("Fred")), ("age", JInt(34)))))
    ))
  val simpleYaml      = """name: Fred
                     |age: 34
                     |""".stripMargin.asInstanceOf[YAML]
  val complexYaml     = """foom: co.blocke.scalajack.Person
                      |thing:
                      |  name: Fred
                      |  age: 34
                      |""".stripMargin.asInstanceOf[YAML]
  val simpleDelimited = "Fred,34".asInstanceOf[DELIMITED]

  test("mapJson") {
    describe(
      "-----------------------------\n:  Converter Mapping Tests  :\n-----------------------------", Console.BLUE
    )
    assertEquals(simpleJson.mapJson[Person](_.copy(age = 45)), """{"name":"Fred","age":45}""".asInstanceOf[JSON])
  }

  test("mapJson4s") {
    assertEquals(simpleJson4s.mapJson4s[Person](_.copy(age = 45)), JObject(List(("name", JString("Fred")), ("age", JInt(45)))))
  }

  test("mapYaml") {
    assertEquals(simpleYaml.mapYaml[Person](_.copy(age = 45)), """name: Fred
                                                              |age: 45
                                                              |""".stripMargin.asInstanceOf[YAML])
  }

  test("mapDelimited") {
    assertEquals(simpleDelimited.mapDelimited[Person](_.copy(age = 45)), """Fred,45""".asInstanceOf[DELIMITED])
  }

  test("mapJsonTo") {
    assertEquals(simpleJson.mapJsonTo[Person, YAML](ScalaJack(YamlFlavor()))(_.copy(age = 45)), """name: Fred
                                                                                              |age: 45
                                                                                              |""".stripMargin.asInstanceOf[YAML])
  }

  test("mapJson4sTo") {
    assertEquals(simpleJson4s.mapJson4sTo[Person, JSON](ScalaJack())(_.copy(age = 45)), """{"name":"Fred","age":45}""".asInstanceOf[JSON])
  }

  test("mapYamlTo") {
    assertEquals(simpleYaml.mapYamlTo[Person, JSON](ScalaJack())(_.copy(age = 45)), """{"name":"Fred","age":45}""".asInstanceOf[JSON])
  }

  test("mapDelimitedTo") {
    assertEquals(simpleDelimited.mapDelimitedTo[Person, JSON](ScalaJack())(_.copy(age = 45)), """{"name":"Fred","age":45}""".asInstanceOf[JSON])
  }

  test("toJson") {
    describe(
      "---------------------------------\n:  Convenience \"to/from\" Tests  :\n---------------------------------", Console.BLUE
    )
    assertEquals(simple.toJson[Person], simpleJson)
    assertEquals(complex.toJson[Typey[Human]], complexJson)
  }

  test("fromJson") {
    assertEquals(simpleJson.fromJson[Person], simple)
    assertEquals(complexJson.fromJson[Typey[Human]], complex)
  }

  test("toJson4s") {
    assertEquals(simple.toJson4s[Person], simpleJson4s)
    assertEquals(complex.toJson4s[Typey[Human]], complexJson4s)
  }

  test("fromJson4s") {
    assertEquals(simpleJson4s.fromJson4s[Person], simple)
    assertEquals(complexJson4s.fromJson4s[Typey[Human]], complex)
  }

  test("toYaml") {
    assertEquals(simple.toYaml[Person], simpleYaml)
    assertEquals(complex.toYaml[Typey[Human]], complexYaml)
  }

  test("fromYaml") {
    assertEquals(simpleYaml.fromYaml[Person], simple)
    assertEquals(complexYaml.fromYaml[Typey[Human]], complex)
  }

  test("toDelimited") {
    assertEquals(simple.toDelimited[Person], simpleDelimited)
  }

  test("fromDelimited") {
    assertEquals(simpleDelimited.fromDelimited[Person], simple)
  }

  test("yamlToJson") {
    describe(
      "----------------------\n:  Converters Tests  :\n----------------------", Console.BLUE
    )
    assertEquals(simpleYaml.yamlToJson[Person], simpleJson)
    assertEquals(complexYaml.yamlToJson[Typey[Human]], complexJson)
  }

  test("yamlToJson4s") {
    assertEquals(simpleYaml.yamlToJson4s[Person], simpleJson4s)
    assertEquals(complexYaml.yamlToJson4s[Typey[Human]], complexJson4s)
  }

  test("jsonToYaml") {
    assertEquals(simpleJson.jsonToYaml[Person], simpleYaml)
    assertEquals(complexJson.jsonToYaml[Typey[Human]], complexYaml)
  }

  test("jsonToJson4s") {
    assertEquals(simpleJson.jsonToJson4s[Person], simpleJson4s)
    assertEquals(complexJson.jsonToJson4s[Typey[Human]], complexJson4s)
  }

  test("delimitedToYaml") {
    assertEquals(simpleDelimited.delimitedToYaml[Person], simpleYaml)
  }

  test("delimitedToJson4s") {
    assertEquals(simpleDelimited.delimitedToJson4s[Person], simpleJson4s)
  }

  test("delimitedToJson") {
    assertEquals(simpleDelimited.delimitedToJson[Person], simpleJson)
  }

  test("json4sToYaml") {
    assertEquals(simpleJson4s.json4sToYaml[Person], simpleYaml)
    assertEquals(complexJson4s.json4sToYaml[Typey[Human]], complexYaml)
  }

  test("json4sToJson") {
    assertEquals(simpleJson4s.json4sToJson[Person], simpleJson)
    assertEquals(complexJson4s.json4sToJson[Typey[Human]], complexJson)
  }
