package co.blocke.scalajack

case class Player(name: String, age: Int)

object Hello extends App {

  val sj = ScalaJack(json4s.Json4sFlavor())

  val m =
    //    Map(Player("Mike", 34) -> 15) //, Map("name" -> "Mike", "age" -> 34) -> 16)
    Map(Map("name" -> "Mike", "age" -> 34) -> 16)
  println(sj.render[Any](m))
}
