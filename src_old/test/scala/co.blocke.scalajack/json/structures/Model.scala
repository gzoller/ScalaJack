package co.blocke.scalajack
package json.structures


// === Eithers
case class Parrot(color: String)
case class DumpTruck(axles: Int)
case class EitherHolder[L, R](either: Either[L, R])

case class Chair(numLegs: Int)
case class Table(numLegs: Int)

trait Pet { val name: String }
case class Dog[A](name: String, kind: A) extends Pet


// === Structures
trait Body
case class FancyBody(message: String) extends Body
case class DefaultBody(message: String = "Unknown body") extends Body
case class AnyBody(stuff: Any) extends Body

trait Hobby
case class InsideHobby(desc: String) extends Hobby

case class Envelope[T <: Body](id: String, body: T) {
  type Giraffe = T
}

// Type member X should be ignored!  Only used internally
case class BigEnvelope[T <: Body, H <: Hobby, X](
    id:    String,
    body:  T,
    hobby: H) {
  type Giraffe = T
  type Hippo = H
  type IgnoreMe = X

  val x: IgnoreMe = null.asInstanceOf[IgnoreMe]
}

case class Bigger(foo: Int, env: Envelope[FancyBody])

// === Unions
case class Person(name: String, age: Int)
case class Multi2(one: List[Boolean] | List[String])
case class Multi3(one: List[String] | List[Int] | Boolean)
case class Multi4(one: List[String] | List[Int] | Boolean | Person)

// === Intersections
trait InterA{ val a: Int }
trait InterB{ val b: Boolean }
trait InterC{ val c: Char }
case class InterImpl(a: Int, b: Boolean, c: Char) extends InterA with InterB with InterC
case class IntersectionHolder( a: InterA & InterB & InterC )