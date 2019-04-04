package co.blocke.scalajack

case class Foo(a: Int, b: Int)
case class Bar()

object RunMe extends App {

  val sj = ScalaJack(delimited.DelimitedFlavor)

  println(sj.render(""))
}
