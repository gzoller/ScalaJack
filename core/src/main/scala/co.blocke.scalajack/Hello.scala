package co.blocke.scalajack

import delimited._

case class Inside(id: Int, desc: String)
case class Nested(thing: String,
                  in: Inside,
                  other: Inside = Inside(99, "dunno"))
case class HasTuples2(one: (String, Inside))

case class A(a: Inside, b: Inside)

object Hello extends App {

  val sj = ScalaJack(DelimitedFlavor())
  val sjx = ScalaJack()

  val c2 = "\"Foo,\"\"3,Bar\"\"\""
  val inst = sj.read[HasTuples2](c2)
  println(inst)
  println(sj.render(inst))
}
