package co.blocke.scalajack
package json
package misc

import org.scalatest.{ FunSpec, Matchers }
import scala.util.Try

case class Foo(
    name:  String,
    stuff: List[String])

class ThreadSafety extends FunSpec with Matchers {

  val sj = ScalaJack()

  describe("-------------------\n:  Thread Safety  :\n-------------------") {
    it("Should not crash when multiple threads access Analyzer (Scala 2.10.x reflection bug)") {
      import scala.concurrent.ExecutionContext.Implicits.global
      import scala.concurrent.duration._
      import scala.concurrent.{ Await, Future }
      import scala.language.postfixOps
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
