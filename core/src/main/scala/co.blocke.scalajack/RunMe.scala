package co.blocke.scalajack

object Size extends Enumeration {
  val Small, Medium, Large = Value
}
case class Shirt(id: Int, size: Size.Value)

object RunMe extends App {

  val sjx = ScalaJack().enumsAsInts()
  val sj = ScalaJack(delimited.DelimitedFlavor).enumsAsInts()

  val s = Shirt(3, Size.Large)
  val js = sjx.render(s)
  println(js)
  println(sjx.read[Shirt](js))

  val d = sj.render(s)
  println(d)
  println(sj.read[Shirt](d))
}
