package co.blocke.scalajack

import delimited._
import json._

case class Person(name: String, age: Int, isOk: Option[Boolean])

object RunMe extends App {

  val sj = ScalaJack(DelimitedFlavor)

  // Nested List
  val csv = """Greg,52,"""
  println(csv)
  val z = sj.read[Person](csv)
  println(z)

  // TODO: WIP -- Optional fields

  /*
  val delim = ','
  val chars = "abc,def".toCharArray
  var i = 0
  while (i < chars.length) {
    chars(i) match {
      case ',' =>
        println(s"($i): D!")
      case c =>
        println(s"($i): $c")
    }
    i += 1
  }
     */

  // Nested Object
  //  val csv = """asdf,"1,2,3",false"""

}

