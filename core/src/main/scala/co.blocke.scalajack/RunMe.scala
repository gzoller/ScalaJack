package co.blocke.scalajack

import delimited._

case class Inside(id: Int, desc: String)
case class HasTuples(one: (String, Int), two: (Boolean, Int) = (true, 1))
case class HasEither(one: Int, two: Either[String, Inside])

object RunMe extends App {

  val sj = ScalaJack(DelimitedFlavor)

  val i = HasTuples(("a\"b", 3), (true, 1))
  println(i)
  println(sj.render(i))
}
