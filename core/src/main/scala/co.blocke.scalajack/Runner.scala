package co.blocke.scalajack

object Size extends Enumeration {
  val Small, Medium, Large = Value
}

case class Person(name: String, age: Int)

object Runner extends App {

  val sj = ScalaJack() //.forType[List[List[String]]]

  val m1: Map[Option[Int], Option[Int]] = Map(Some(3) -> None)
  val m2: Map[Option[Int], Option[Int]] = Map(None -> Some(2), Some(5) -> null)
  val inst = Map(Map(m1 -> m2) -> Map(m2 -> m1))
  val js = sj.render(inst)
  println(js)
  println(sj.read[Map[Map[Map[Option[Int], Option[Int]], Map[Option[Int], Option[Int]]], Map[Map[Option[Int], Option[Int]], Map[Option[Int], Option[Int]]]]](js))
  //  assertResult("""{"{\"{}\":{\"5\":null}}":{"{\"5\":null}":{}}}""") { js }
  //  assertResult(Map(Map(Map() -> Map(None -> Some(2), Some(5) -> None)) -> Map(Map(None -> Some(2), Some(5) -> None) -> Map()))) {
  //    sj.read[Map[Map[Map[Option[Int], Option[Int]], Map[Option[Int], Option[Int]]], Map[Map[Option[Int], Option[Int]], Map[Option[Int], Option[Int]]]]](js)
  //  }

}
