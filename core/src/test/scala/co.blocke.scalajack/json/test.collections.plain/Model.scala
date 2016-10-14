package co.blocke.scalajack
package json.test.collections.plain

class PlayerVal(val name: String, val age: Int)
class PlayerMix() {
  def someConfusingThing() = true
  var name: String = "" // public var member

  private var _age: Int = 0
  def age: Int = _age // getter/setter member
  def age_=(a: Int) = _age = a
}

// class OptionBigInt(o: Option[BigInt])
// class OptionClass(name: String, age: Option[Int])
// class OptionTuple(foo: Int, t: (Boolean, Option[String], Int))

// trait Person { val name: String }
// class SomeClass(name: String, age: Int) extends Person

// trait Thing[A, B] { val a: A; val b: B }
// class AThing[Y, X](a: X, b: Y) extends Thing[X, Y]

