package co.blocke.scalajack

import json._

case class Person(name: String, age: Int)

object Runner extends App {

  val sj = ScalaJack().forType[List[List[String]]]

  println(sj.fastRead("""[["a","b","c"],["a","b","c"]]"""))

  println(sj.read[Map[String, Int]]("""{"a":1,"b":2}"""))

  println(sj.read[Map[List[Int], Boolean]]("""{"[1,2,3]":true,"[4,5,6]":false}"""))

  println(sj.read[Person]("""{"name":"Greg","age":52}"""))

  //  val t = JsonTokenizer()
  //  val tok = t.tokenize("""{"name":"Greg\"Zoller","age":52}""")
  //  println(tok.toList)
}
