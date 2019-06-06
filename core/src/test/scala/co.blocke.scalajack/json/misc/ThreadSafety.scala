package co.blocke.scalajack
package json
package misc

import org.scalatest.Matchers
import org.scalatest.funspec.AnyFunSpec
import scala.util.Try

case class Foo(
    name:  String,
    stuff: List[String])

class ThreadSafety extends AnyFunSpec with Matchers {

  val sj = ScalaJack()

  describe("-------------------\n:  Thread Safety  :\n-------------------") {
    it("Should not crash when multiple threads access Analyzer (Scala 2.10.x reflection bug)") {
      import scala.concurrent.ExecutionContext.Implicits.global
      import scala.concurrent.duration._
      import scala.concurrent.{ Await, Future }

      val doit = () =>
        Try {
          val js = sj.render(Foo("Greg", List("a", "b", "c")))
          sj.read[Foo](js)
        }.toOption.isDefined
      val z = List(
        Future(doit()),
        Future(doit()),
        Future(doit()),
        Future(doit()))
      val res = Await.result(Future.sequence(z), 3 seconds).reduce((a, b) => a && b)
      res should be(true)
    }
  }
}
