package co.blocke.scalajack
package json.test.misc

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

// --- For Self-Reference tests

case class HooLoo(
  name: String,
  more: HooLoo
)

case class HooLoo2[T](
  name: String,
  x:    T,
  more: HooLoo2[Int]
)

case class HooLoo3[T](
  name: String,
  x:    T,
  more: HooLoo3[T]
)

case class HooLoo4(
  name: String,
  more: Option[HooLoo4]
)

case class HooLoo5(
  name: String,
  more: List[HooLoo5]
)

case class HooLoo6[T](
  name: String,
  x:    T,
  more: List[HooLoo6[T]]
)

// --- Complex Relationships

trait Parent[A, B] { val a: A; val b: B }
case class Child[A, B, C](a: A, b: B, c: C) extends Parent[A, B]

object Kind extends Enumeration {
  val Lab, Pug = Value
}
trait Pet { val name: String }
case class Dog[A](name: String, kind: A) extends Pet

case class HasDefaults(name: String, age: Option[Int], pet: Pet = Dog("Fido", true))

@Collection(name = "myDefaults")
case class DefaultOpt(
  @DBKey(index = 1) name:String,
  age:                  Option[Int] = Some(19)
)

@Collection(name = "plains")
class Plain() {
  @DBKey(index = 1) var name: String = "" // public var member
  var age: Option[Int] = Some(19)
}

case class MapFactor(
  @MapName(name = "foo_bar") fooBar:String,
  @MapName(name = "a_b") thingy:   Long,
  count:                           Int
)
class MapFactorPlain() {
  @MapName(name = "foo_bar") var fooBar: String = ""
  @MapName(name = "a_b") var thingy: Long = 0L
  var count: Int = 0
}