package co.blocke.scalajack

import delimited._
import json._

case class Foo(id: String, count: Int)
case class Person(name: String, age: Int, stuff: Foo)

object RunMe extends App {

  val sj = ScalaJack(DelimitedFlavor.apply('|'))

  val p = Person("Greg", 52, Foo("Hey", 5))
  val csv = sj.render(p)
  println("1> " + csv)
  val z = sj.read[Person](csv)
  println("2> " + z)

  println("3> " + sj.render(z))

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

