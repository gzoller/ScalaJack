package co.blocke.scalajack

object Size extends Enumeration {
  val Small, Medium, Large = Value
}

case class Person(name: String, age: Int)

object Runner extends App {

  val sj = ScalaJack() //.forType[List[List[String]]]

  val m1: Any = Map(List(1, 2, 3) -> List("a", "b", "c"))
  println(sj.render(m1))

}
