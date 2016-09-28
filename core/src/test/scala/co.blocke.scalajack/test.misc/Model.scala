package co.blocke.scalajack
package test
package misc

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
    age:      Int
) {
  val foo: String = "yikes!"
}
case class Encapsulated(
  foo: String,
  bar: Boolean
)
case class View1(
  name:  String,
  big:   Long,
  maybe: Option[String]
)
case class View2(
  name:     String,
  flipflop: Boolean,
  mymap:    Map[String, Int]
)

case class Partial(
  name:  String,
  bogus: Int
)
case class Empty(
  name:  String,
  stuff: List[String] // should be empty in test
)

case class NoMatch(
  bogus: Boolean,
  nah:   Int
)
