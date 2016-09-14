package co.blocke.scalajack
package test.v4

import org.scalatest.FunSpec
import org.scalatest.Matchers._
import scala.language.postfixOps
import scala.util.Try
import org.joda.time.{ DateTime, DateTimeZone }
import org.joda.time.format.DateTimeFormat

class JsonMapSpec extends FunSpec {

  val sj = ScalaJack()

  describe("==================\n| -- V4 Tests -- |\n==================") {
    describe("JSON-to-Collection tests") {
      it("Simple Map") {
        val js = """{"name":"Greg","age":49,"likesFood":true,"balance":12.32}"""
        sj.read[Map[String, Any]](js) should equal(Map("name" -> "Greg", "age" -> 49, "likesFood" -> true, "balance" -> 12.32))
      }
      it("Simple List") {
        val js = """[12.34,54.2,0.2]"""
        sj.read[List[Double]](js) should equal(List(12.34, 54.2, 0.2))
      }
      it("Complex Map") {
        val js = """{"kind":{"name":"Greg","stuff":[1,2,3]},"other":["a","b","c"]}"""
        sj.read[Map[String, Any]](js) should equal(Map("kind" -> Map("name" -> "Greg", "stuff" -> List(1, 2, 3)), "other" -> List("a", "b", "c")))
      }
      it("Complex List of Lists") {
        val js = """[[1,2,3],[4,5,6],[7,8,9]]"""
        sj.read[List[List[Int]]](js) should equal(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))
      }
      it("Complex List of Maps") {
        val js = """[{"foo":"bar","hey":true},{"foo":"wow","hey":false}]"""
        sj.read[List[Map[String, Any]]](js) should equal(List(Map("foo" -> "bar", "hey" -> true), Map("foo" -> "wow", "hey" -> false)))
      }
    }
  }
}