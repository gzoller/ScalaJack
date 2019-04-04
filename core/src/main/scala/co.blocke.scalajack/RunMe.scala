package co.blocke.scalajack

object Size extends Enumeration {
  val Small, Medium, Large = Value
}
case class Shirt(id: Int, size: Size.Value)

object RunMe extends App {

  val sjx = ScalaJack()
  val sj = ScalaJack(delimited.DelimitedFlavor).enumsAsInts()

  val t = sjx.context.typeAdapterOf[String]
  t.as[model.TypeAdapter[List[Int]]]
}
