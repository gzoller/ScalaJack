package co.blocke.scalajack

object Runme extends App {

  val sj = ScalaJack()

  val master = Master("Greg", List("a", "b"), List(Encapsulated("x", false), Encapsulated("y", true)), Encapsulated("Nest!", true), Some("wow"), Map("hey" -> 17, "you" -> 21), true, 99123986123L, Num.C, 46)
  val x = sj.view[View1](master)
  println(x)
  //  val y: Master = sj.spliceInto(x.copy(name = "Fred", big = 2L), master)
  //  println(y)
}

object Num extends Enumeration {
  val A, B, C = Value
}
case class Master(
    name:     String,
    stuff:    List[String],
    more:     List[Encapsulated],
    nest:     Encapsulated,
    maybe:    Option[String],
    mymap:    Map[String, Int],
    flipflop: Boolean,
    big:      Long,
    num:      Num.Value,
    age:      Int) {
  val foo: String = "yikes!"
}
case class Encapsulated(
    foo: String,
    bar: Boolean)
case class View1(
    name:  String,
    big:   Long,
    maybe: Option[String])