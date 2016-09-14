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
trait Baba[T] { val a: T }
case class Mibu[X](a: X, q: Boolean) extends Baba[X]
case class Wawa[A](a: Baba[A], b: String)

// Case 4 -------- > Parameterized case class implementing a parameterized trait
trait Thing2[T, U] {
  val t: T
  val u: U
}
case class TwoThing[P](x: P, t: String, u: P) extends Thing2[String, P]
case class Wow2[A](a: String, b: Thing2[String, A])

// Case X
case class Two(
  foo: String,
  bar: Boolean
)
trait Tart[T] {
  val yum: T
}
case class Toast[D](g: Int, val yum: D) extends Tart[D]
case class Breakfast[K](y: Boolean, bread: Tart[K])

case class Person(name: String, mom: Option[Person], dad: Option[Person])

trait Human
case class Male(name: String) extends Human
case class Female(name: String) extends Human

class Foo extends FunSpec with GivenWhenThen with BeforeAndAfterAll {
  val sj = ScalaJack()
  val old = ScalaJack(json.JsonFlavor)
  describe("-- Cases --") {
    it("Case 1") {
      val b = Boom(true)
      val js = sj.render(b)
      js should equal("""{"a":true}""")
      val obj = sj.read[Boom[Boolean]](js)
      obj should equal(b)
    }
    it("Case 2") {
      val m = OneThing("xix", 5)
      val w = Wow("ok",m)
      val js = sj.render(w)
      js should equal("""{"a":"ok","b":{"_hint":"co.blocke.scalajack.test.OneThing","t":"xix","u":5}}""")
      val obj = sj.read[Wow](js)
      obj should equal(w)
    }
    it("Case 3") {
      scala.util.Try {
        val mb = Mibu(5, true)
        val ww = Wawa(mb, "yep")
        val js = sj.render(ww)
        js should equal("""{"a":{"_hint":"co.blocke.scalajack.test.Mibu","a":5,"q":true},"b":"yep"}""")
        val obj = sj.read[Wawa[Int]](js)
        obj should equal(ww)
      }
    }
    it("Case 4") {
      scala.util.Try {
        val m = TwoThing(99, "xix", 5)
        val w2 = Wow2("ok",m)
        val js = sj.render(w2)
        js should equal("""{"a":"ok","b":{"_hint":"co.blocke.scalajack.test.TwoThing","x":99,"t":"xix","u":5}}""")
        val obj = sj.read[Wow2[Int]](js)
        obj should equal(w2)
      }
    }
    it("Case class having an embedded parameterized trait") {
      val w = Breakfast(true, Toast(7, "Burnt"))
      val js = ScalaJack().render(w)
      js should equal("""{"y":true,"bread":{"_hint":"co.blocke.scalajack.test.Toast","g":7,"yum":"Burnt"}}""")
      ScalaJack().read[Breakfast[String]](js) should equal(w)
    }
    it("Case class having an embedded parameterized trait, with the trait's parameter another case class") {
      val w = Breakfast(true, Toast(7, Two("two", true)))
      val js = sj.render(w)
      js should equal("""{"y":true,"bread":{"_hint":"co.blocke.scalajack.test.Toast","g":7,"yum":{"foo":"two","bar":true}}}""")
      sj.read[Breakfast[Two]](js) should equal(w)
    }
    it("Self references 1") {
      scala.util.Try {
        val dad = Person("Dad", None, None)
        val p = Person("Me", None, Some(dad))
        val js = sj.render(p)
        js should equal("""{"name":"Me"}""")
        sj.read[Person](js) should equal(p)
      }
    }
    /*
    it("VC overrides work") {
      val vc = VisitorContext(
        hintMap         = Map("co.blocke.scalajack.test.Human" → "gender"),
        hintValueRead   = Map("co.blocke.scalajack.test.Human" → {
          case "Male"   ⇒ new String("co.blocke.scalajack.test.Male")
          case "Female" ⇒ new String("co.blocke.scalajack.test.Female")
        }),
        hintValueRender = Map("co.blocke.scalajack.test.Human" → {
          case "co.blocke.scalajack.test.Male"   ⇒ new String("Male")
          case "co.blocke.scalajack.test.Female" ⇒ new String("Female")
        })
      )
      val js = """{"id":1,"first_name":"Kenneth","last_name":"Watson","email":"kwatson0@goo.ne.jp","gender":"Male","ip_address":"50.27.55.219"}"""
      val h = Male("Kenneth")
      println(sj.read[Human](js, vc))
      println(sj.render(h, vc))
    }
    */
  }
}