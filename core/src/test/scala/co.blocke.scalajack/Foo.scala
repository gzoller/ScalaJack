package co.blocke.scalajack
package test

import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
import org.scalatest.Matchers._

// Case 1 -------- > Simple parameterized case class
case class Boom[T](a: T)

// Case 2 -------- > Class having a parameterized trait
trait Thing[T, U] {
  val t: T
  val u: U
}
case class OneThing(t: String, u: Int) extends Thing[String, Int]
case class Wow(a: String, b: Thing[String, Int])

// Case 3 -------- > Parameterized class having a parameterized trait
trait Thing2[T, U] {
  val t: T
  val u: U
}
case class TwoThing[P](x: List[P], t: String, u: P) extends Thing2[String, P]
case class Wow2[A](a: String, b: Thing2[String, A])

// Case 4 -------- > Parameterized case class implementing a parameterized trait
trait Xis[T, U] {
  val a: T
  val b: U
}
case class Mine[A, B](a: A, b: B) extends Xis[A, B]

class Foo extends FunSpec with GivenWhenThen with BeforeAndAfterAll {
  val sj = ScalaJack()
  val old = ScalaJack(json.JsonFlavor())
  describe("--1--") {
    // it("Works") {
    //   val m = OneThing("xix", 5)
    //   val js = sj.render(Wow("ok", m))
    //   println(js)
    //   val obj = sj.read[Wow](js)
    //   println(obj)
    // }
    // it("Old") {
    //   val m = TwoThing(1, "xix", 5)
    //   val js = old.render(Wow2("ok", m))
    //   println(js)
    //   val obj = old.read[Wow2[Int]](js)
    //   println(obj)
    // }
    it("Works 2") {
      scala.util.Try {
        val m = TwoThing(List(1, 2), "xix", 5)
        val js = sj.render(Wow2("ok", m))
        println(js)
        val obj = sj.read[Wow2[Int]](js)
        println(obj)
      }
    }
  }
}