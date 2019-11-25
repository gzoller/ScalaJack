package co.blocke.scalajack

import delimited._

case class Person(name: String, stuff: List[List[Int]], pet: Pet, age: Int)
case class Pet(name: String, numLets: Int)

object Food extends Enumeration {
  val Seeds, Meat, Pellets, Veggies = Value
}
case class SampleTupleOptional(
    m: Map[(Option[Int], Option[String]), (Option[Int], Option[Food.Value])]
)

object Hello extends App {

  val sj = ScalaJack(DelimitedFlavor('|'))
  val csv =
    sj.render(
      Person(
        "Zoller\",\"Greg",
        List(List(1, 2, 3), List(4, 5, 6)),
        Pet("Rosie", 4),
        53
      )
    )
  println(csv)
  println(sj.read[Person](csv))

  /*
  //  val dp = delimited.DelimitedParser('|', "\"1|2\"", null)
  //  println(s">${dp.expectString()}<")
  //  println(s">${dp.expectString()}<")

  val dp = delimited.DelimitedParser('|', "\"true|5\"", null)
  val ta = sj.taCache.typeAdapterOf[(Boolean, Int)]
  //  val builder = scala.collection.mutable
  //    .ListBuffer[Int]()
  //    .asInstanceOf[scala.collection.mutable.Builder[Int, List[Int]]]
  println(
    s">${dp.expectTuple(ta.asInstanceOf[typeadapter.TupleTypeAdapter[_]].fields)}<"
  )

  //  val s = """t""r|"x|""y|z""|1"|a|b|"c|d""""
  //  val dp = delimited.DelimitedParser('|', s, null)
  //  (0 to 4).foreach(_ => println(dp.expectToken))
 */

}
