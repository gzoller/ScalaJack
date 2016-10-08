package co.blocke.scalajack
package json.test.misc

import org.scalatest.{ FunSpec, Matchers }
import java.util.UUID
import scala.reflect.runtime.universe.typeOf

class ViewSplice() extends FunSpec with Matchers {

  val sj = ScalaJack()

  describe("-----------------------\n:  View/Splice Tests  :\n-----------------------") {
    it("Must process view") {
      val master = Master("Greg", List("a", "b"), List(Encapsulated("x", false), Encapsulated("y", true)), Encapsulated("Nest!", true), Some("wow"), Map("hey" -> 17, "you" -> 21), true, 99123986123L, Num.C, 46)
      sj.view[View2](master) should equal(View2("Greg", true, Map("hey" -> 17, "you" -> 21)))
    }
    it("Must process empty collections in view") {
      val master = Master("Greg", List(), List(Encapsulated("x", false), Encapsulated("y", true)), Encapsulated("Nest!", true), Some("wow"), Map("hey" -> 17, "you" -> 21), true, 99123986123L, Num.C, 46)
      sj.view[Empty](master) should equal(Empty("Greg", List.empty[String]))
    }
    it("Must spliceInto") {
      val master = Master("Greg", List("a", "b"), List(Encapsulated("x", false), Encapsulated("y", true)), Encapsulated("Nest!", true), Some("wow"), Map("hey" -> 17, "you" -> 21), true, 99123986123L, Num.C, 46)
      val x = sj.view[View1](master)
      val y: Master = sj.spliceInto(x.copy(name = "Fred", big = 2L), master)
      y should equal(master.copy(name = "Fred", big = 2L))
    }
    it("Must spliceInto with empty collection from view") {
      val master = Master("Greg", List("a", "b"), List(Encapsulated("x", false), Encapsulated("y", true)), Encapsulated("Nest!", true), Some("wow"), Map("hey" -> 17, "you" -> 21), true, 99123986123L, Num.C, 46)
      val x = Empty("Greg", List.empty[String])
      val y: Master = sj.spliceInto(x, master)
      y should equal(master.copy(stuff = List.empty[String]))
    }
    it("Splicing in an object where not all fields match must splice in the compatible fields") {
      val master = Master("Greg", List("a", "b"), List(Encapsulated("x", false), Encapsulated("y", true)), Encapsulated("Nest!", true), Some("wow"), Map("hey" -> 17, "you" -> 21), true, 99123986123L, Num.C, 46)
      val x = Partial("Mike", 50)
      val y: Master = sj.spliceInto(x, master)
      y should equal(master.copy(name = "Mike"))
    }
    it("Splicing in an incompatible object shall nave no impact on the master") {
      val master = Master("Greg", List("a", "b"), List(Encapsulated("x", false), Encapsulated("y", true)), Encapsulated("Nest!", true), Some("wow"), Map("hey" -> 17, "you" -> 21), true, 99123986123L, Num.C, 46)
      val x = NoMatch(true, 25)
      val y: Master = sj.spliceInto(x, master)
      y should equal(master)
    }
    it("Must enforce View object as a case class") {
      val master = Master("Greg", List("a", "b"), List(Encapsulated("x", false), Encapsulated("y", true)), Encapsulated("Nest!", true), Some("wow"), Map("hey" -> 17, "you" -> 21), true, 99123986123L, Num.C, 46)
      the[ViewException] thrownBy sj.view[Int](master) should have message """Output of view() must be a case class.  scala.Int is not a case class."""
    }
  }
}
