package co.blocke.scalajack

object Food extends Enumeration {
  val Seeds, Meat, Pellets, Veggies = Value
}
case class SampleTupleOptional(
    m: Map[(Option[Int], Option[String]), (Option[Boolean], Option[Food.Value])]
)

object Hello extends App {

  val sj = ScalaJack()

  val a = (Some(5), Some("Fred"))
  val b = (None, Some(Food.Meat))
  val inst = SampleTupleOptional(Map(a -> b))

  println(sj.taCache.typeAdapterOf[Int])
  println(sj.taCache.typeAdapterOf[Option[Int]])
  //  println(sj.taCache.typeAdapterOf[(Option[Int], Option[String])])

  //  val js = sj.render(inst)
  //  println(js)

  //  assertResult("""{"m":{"[5,\"Fred\"]":[null,"Meat"]}}""") { js }
  //  assertResult(inst) {
  //    sj.read[SampleTupleOptional](js)
  //  }
}
