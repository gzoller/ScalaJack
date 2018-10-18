package co.blocke.scalajack
package json.test.collections

case class Player(name: String, age: Int)

case class OptionBigInt(o: Option[BigInt])
case class OptionClass(name: String, age: Option[Int])
case class OptionTuple(foo: Int, t: (Boolean, Option[String], Int))
trait Person { val name: String }
case class SomeClass(name: String, age: Int) extends Person
trait Thing[A, B] { val a: A; val b: B }
case class AThing[Y, X](a: X, b: Y) extends Thing[X, Y]

