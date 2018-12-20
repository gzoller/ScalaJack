package co.blocke.scalajack

import json._

case class Person(name: String, age: Int, other: Option[Long])
trait Pet {
  val numLegs: Int
  val name: String
}
case class Dog(name: String, numLegs: Int, weight: Double)

object Runner extends App {

  val sj = ScalaJack().forType[List[List[String]]]

  println(sj.fastRead("""[["a","b","c"],["a","b","c"]]"""))

  println(sj.read[Map[String, Int]]("""{"a":1,"b":2}"""))

  println(sj.read[Map[List[Int], Boolean]]("""{"[1,2,3]":true,"[4,5,6]":false}"""))

  println(sj.read[Person]("""{"name":"Greg","age":52}"""))

  println(sj.read[Pet]("""{"name":"George","numLegs":4,"_hint":"co.blocke.scalajack.Dog","weight": 12.34}"""))

  println(sj.read[Map[Any, Int]]("""{"[1,2,3]":25}"""))

  println(sj.read[Any]("""{"name":"George","numLegs":4,"_hint":"co.blocke.scalajack.Dog","weight": 12.34}"""))

  //  val t = JsonTokenizer()
  //  val tok = t.tokenize("""{"name":"Greg\"Zoller","age":52}""")
  //  println(tok.toList)
}
