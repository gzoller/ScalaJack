package co.blocke.scalajack
package test
package collections

import org.scalatest.{ FunSpec, Matchers }
import scala.reflect.runtime.universe.typeOf

case class OptionBigInt(o: Option[BigInt])
case class OptionClass(name: String, age: Option[Int])
case class OptionTuple(foo: Int, t: (Boolean, Option[String], Int))

class Options() extends FunSpec with Matchers {

  val sj = ScalaJack()

  describe("------------------\n:  Option Tests  :\n------------------") {
    describe("+++ Positive Tests +++") {
      it("Option of BigInt (naked)") {
        val inst: Option[BigInt] = Some(BigInt(5))
        val js = sj.render(inst)
        assertResult("5") { js }
        assertResult(inst) {
          sj.read[Option[BigInt]](js)
        }
      }
      it("Option of BigInt (in class)") {
        val inst = OptionBigInt(Some(BigInt(5)))
        val js = sj.render(inst)
        assertResult("""{"o":5}""") { js }
        assertResult(inst) {
          sj.read[OptionBigInt](js)
        }
      }
      // <sigh>  One type is enough here.  Not going thru all the primitive variants again... It works.
      it("Option is None (in class)") {
        val inst = OptionClass("Mike", None)
        val js = sj.render(inst)
        assertResult("""{"name":"Mike"}""") { js }
        assertResult(inst) {
          sj.read[OptionClass](js)
        }
      }
      it("Option is None (in List)") {
        val inst: List[Option[Int]] = List(Some(1), None, Some(2))
        val js = sj.render(inst)
        assertResult("""[1,2]""") { js }
        assertResult(List(Some(1), Some(2))) { // The None gets erased here
          sj.read[List[Option[Int]]](js)
        }
      }
      it("Option is None (value in Map)") {
        val inst: Map[Int, Option[String]] = Map(1 -> Some("one"), 2 -> None, 3 -> Some("three"))
        val js = sj.render(inst)
        assertResult("""{"1":"one","3":"three"}""") { js }
        assertResult(Map(1 -> Some("one"), 3 -> Some("three"))) { // The None gets erased here
          sj.read[Map[Int, Option[String]]](js)
        }
      }
      it("Option is None (key in Map)") {
        val inst: Map[Option[String], Int] = Map(Some("one") -> 1, None -> 2, Some("three") -> 3)
        val js = sj.render(inst)
        assertResult("""{"one":1,"":2,"three":3}""") { js }
        assertResult(inst) { // The None gets erased here
          sj.read[Map[Option[String], Int]](js)
        }
      }
      it("Option is None (in Tuple)") {
        val inst = List(OptionTuple(1, (true, Some("ok"), 2)), OptionTuple(5, (false, None, 3)))
        val js = sj.render(inst)
        assertResult("""[{"foo":1,"t":[true,"ok",2]},{"foo":5,"t":[false,null,3]}]""") { js }
        assertResult(inst) { // The None gets erased here
          sj.read[List[OptionTuple]](js)
        }
      }
      it("Reading null into optional (naked)") {
        (pending)
      }
      it("Reading null into optional class field") {
        (pending)
      }
      it("Reading null into optional List item") {
        (pending)
      }
      it("Reading null into optional Map item") {
        (pending)
      }
      it("Reading null into optional Tuple item") {
        (pending)
      }
    }
  }
}