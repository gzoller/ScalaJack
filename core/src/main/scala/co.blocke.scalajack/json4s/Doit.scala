package co.blocke.scalajack
package json4s

import org.json4s._

case class Person(name: String, age: Int, stuff: Map[Int, String])

object Doit extends App {

  val sj = ScalaJack(Json4sFlavor())

  val j = sj.render(Person("Greg", 52, Map(1 -> "a", 2 -> "b")))
  println(j)

  val s = JString("Foo")
  println(sj.read[Person](j))
}
