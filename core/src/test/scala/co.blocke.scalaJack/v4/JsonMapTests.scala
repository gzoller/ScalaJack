package co.blocke.scalajack
package test.v4

import org.scalatest.FunSpec
import org.scalatest.Matchers._
import scala.language.postfixOps
import scala.util.Try
import org.joda.time.{ DateTime, DateTimeZone }
import org.joda.time.format.DateTimeFormat

class JsonMapSpec extends FunSpec {

  describe("==================\n| -- V4 Tests -- |\n==================") {
    describe("JSON-to-Collection tests") {
      it("Simple Map") {
        val js = """{"name":"Greg","age":49,"likesFood":true,"balance":12.32}"""
        JSON.toCollection(js).left.get should equal(Map("name" -> "Greg", "age" -> 49, "likesFood" -> true, "balance" -> 12.32))
      }
      it("Simple List") {
        val js = """[12.34,54.2,0.2]"""
        JSON.toCollection(js).right.get should equal(List(12.34, 54.2, 0.2))
      }
      it("Complex Map") {
        val js = """{"kind":{"name":"Greg","stuff":[1,2,3]},"other":["a","b","c"]}"""
        JSON.toCollection(js).left.get should equal(Map("kind" -> Map("name" -> "Greg", "stuff" -> List(1, 2, 3)), "other" -> List("a", "b", "c")))
      }
      it("Complex List of Lists") {
        val js = """[[1,2,3],[4,5,6],[7,8,9]]"""
        JSON.toCollection(js).right.get should equal(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))
      }
      it("Complex List of Maps") {
        val js = """[{"foo":"bar","hey":true},{"foo":"wow","hey":false}]"""
        JSON.toCollection(js).right.get should equal(List(Map("foo" -> "bar", "hey" -> true), Map("foo" -> "wow", "hey" -> false)))
      }
    }
  }
}