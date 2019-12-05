package co.blocke.scalajack
package json.misc

import org.scalatest.Matchers
import org.scalatest.funspec.AnyFunSpec

class ViewSpliceSpec() extends AnyFunSpec with Matchers {

  val sj = ScalaJack()

  describe(
    "-----------------------\n:  View/Splice Tests  :\n-----------------------"
  ) {
      it("Must process view") {
        val master = Master(
          "Greg",
          List("a", "b"),
          List(Encapsulated("x", bar = false), Encapsulated("y", bar = true)),
          Encapsulated("Nest!", bar = true),
          Some("wow"),
          Map("hey" -> 17, "you" -> 21),
          flipflop = true,
          99123986123L,
          Num.C,
          46
        )
        sj.view[View2](master) should equal(
          View2("Greg", flipflop = true, Map("hey" -> 17, "you" -> 21))
        )
      }
      it("Must process empty collections in view") {
        val master = Master(
          "Greg",
          List(),
          List(Encapsulated("x", bar = false), Encapsulated("y", bar = true)),
          Encapsulated("Nest!", bar = true),
          Some("wow"),
          Map("hey" -> 17, "you" -> 21),
          flipflop = true,
          99123986123L,
          Num.C,
          46
        )
        sj.view[Empty](master) should equal(Empty("Greg", List.empty[String]))
      }
      it("Must enforce View object as a case class") {
        val master = Master(
          "Greg",
          List("a", "b"),
          List(Encapsulated("x", bar = false), Encapsulated("y", bar = true)),
          Encapsulated("Nest!", bar = true),
          Some("wow"),
          Map("hey" -> 17, "you" -> 21),
          flipflop = true,
          99123986123L,
          Num.C,
          46
        )
        the[ScalaJackError] thrownBy sj.view[Int](master) should have message """Output of view() must be a case class.  scala.Int is not a case class."""
      }
      it("Must enforce required constructor fields") {
        val master = Master(
          "Greg",
          List("a", "b"),
          List(Encapsulated("x", bar = false), Encapsulated("y", bar = true)),
          Encapsulated("Nest!", bar = true),
          Some("wow"),
          Map("hey" -> 17, "you" -> 21),
          flipflop = true,
          99123986123L,
          Num.C,
          46
        )
        val msg =
          """View master object co.blocke.scalajack.json.misc.Master is missing field bar required to build view object Encapsulated"""
        the[ScalaJackError] thrownBy sj.view[Encapsulated](master) should have message msg
      }
      it("Must spliceInto") {
        val master = Master(
          "Greg",
          List("a", "b"),
          List(Encapsulated("x", bar = false), Encapsulated("y", bar = true)),
          Encapsulated("Nest!", bar = true),
          Some("wow"),
          Map("hey" -> 17, "you" -> 21),
          flipflop = true,
          99123986123L,
          Num.C,
          46
        )
        val x = sj.view[View1](master)
        val y: Master = sj.spliceInto(x.copy(name = "Fred", big = 2L), master)
        y should equal(master.copy(name = "Fred", big = 2L))
      }
      it("Must enforce spliceInto target is a case class") {
        val master = Master(
          "Greg",
          List("a", "b"),
          List(Encapsulated("x", bar = false), Encapsulated("y", bar = true)),
          Encapsulated("Nest!", bar = true),
          Some("wow"),
          Map("hey" -> 17, "you" -> 21),
          flipflop = true,
          99123986123L,
          Num.C,
          46
        )
        val x = sj.view[View1](master)
        the[ScalaJackError] thrownBy sj.spliceInto(
          x.copy(name = "Fred", big = 2L),
          "some non-case-class"
        ) should have message """Output of spliceInto() must be a case class.  java.lang.String is not a case class."""
      }
      it("Must spliceInto with empty collection from view") {
        val master = Master(
          "Greg",
          List("a", "b"),
          List(Encapsulated("x", bar = false), Encapsulated("y", bar = true)),
          Encapsulated("Nest!", bar = true),
          Some("wow"),
          Map("hey" -> 17, "you" -> 21),
          flipflop = true,
          99123986123L,
          Num.C,
          46
        )
        val x = Empty("Greg", List.empty[String])
        val y: Master = sj.spliceInto(x, master)
        y should equal(master.copy(stuff = List.empty[String]))
      }
      it(
        "Splicing in an object where not all fields match must splice in the compatible fields"
      ) {
          val master = Master(
            "Greg",
            List("a", "b"),
            List(Encapsulated("x", bar = false), Encapsulated("y", bar = true)),
            Encapsulated("Nest!", bar = true),
            Some("wow"),
            Map("hey" -> 17, "you" -> 21),
            flipflop = true,
            99123986123L,
            Num.C,
            46
          )
          val x = Partial("Mike", 50)
          val y: Master = sj.spliceInto(x, master)
          y should equal(master.copy(name = "Mike"))
        }
      it("Splicing in an incompatible object shall nave no impact on the master") {
        val master = Master(
          "Greg",
          List("a", "b"),
          List(Encapsulated("x", bar = false), Encapsulated("y", bar = true)),
          Encapsulated("Nest!", bar = true),
          Some("wow"),
          Map("hey" -> 17, "you" -> 21),
          flipflop = true,
          99123986123L,
          Num.C,
          46
        )
        val x = NoMatch(bogus = true, 25)
        val y: Master = sj.spliceInto(x, master)
        y should equal(master)
      }
    }
}
