package co.blocke.scalajack
package test
package misc

import org.scalatest.{ FunSpec, Matchers }
import java.util.UUID
import scala.reflect.runtime.universe.typeOf

import scala.util._

object MyStatic {}

case class Embed(stuff: List[String], num: Int)
case class Boom(
  name:  String,
  other: Try[Embed]
)

class TryAndCapture() extends FunSpec with Matchers {

  val sj = ScalaJack()

  describe("---------------------------\n:  Try and Capture Tests  :\n---------------------------") {
    it("Try sucess") {
      val js = """{"name":"Greg","other":{"stuff":["a","b","c"],"num":2}}"""
      val obj = sj.read[Boom](js)
      assertResult(Boom("Greg", Success(Embed(List("a", "b", "c"), 2)))) { obj }
      assertResult("""{"name":"Greg","other":{"stuff":["a","b","c"],"num":2}}""") { sj.render(obj) }
    }
    it("Try failure") {
      val js = """{"name":"Greg","other":50}"""
      val obj = sj.read[Boom](js)
      assertResult("scala.MatchError: Number (of class scala.Enumeration$Val)") { obj.other.asInstanceOf[Failure[_]].exception.getMessage }
      assertResult("""{"name":"Greg","other":null}""") { sj.render(obj) }
    }
  }
}
