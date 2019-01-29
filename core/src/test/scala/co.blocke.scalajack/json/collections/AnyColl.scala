package co.blocke.scalajack
package json.test.collections

import org.scalatest.{ FunSpec, Matchers }

class AnyColl() extends FunSpec with Matchers {

  val sj = ScalaJack()

  describe("--------------------------\n:  Any Collection Tests  :\n--------------------------") {
    it("List works (Int)") {
      val inst: Any = List(1, 2, 3)
      val js = sj.render(inst)
      assertResult("""[1,2,3]""") { js }
      assertResult(List(1, 2, 3)) {
        sj.read[Any](js)
      }
    }
    it("List works (String)") {
      val inst: Any = List("one", "two", "three")
      val js = sj.render(inst)
      assertResult("""["one","two","three"]""") { js }
      assertResult(List("one", "two", "three")) {
        sj.read[Any](js)
      }
    }
    /*
    it("List works (Class)") {
      val inst: Any = List(Player("Mike", 34), Player("Sarah", 29))
      val js = sj.render[Any](inst)
      println(js)
      println(sj.read[List[Any]](js))
      assertResult("""[{"_hint":"co.blocke.scalajack.json.test.collections.Player","name":"Mike","age":34},{"_hint":"co.blocke.scalajack.json.test.collections.Player","name":"Sarah","age":29}]""") { js }
      assertResult(List(Player("Mike", 34), Player("Sarah", 29))) {
        sj.read[List[Any]](js)
      }
    }
    */
    it("Map works (Int,Int)") {
      val inst: Any = Map(1 -> 2, 3 -> 4)
      val js = sj.render(inst)
      println(js)
      assertResult("""{"1":2,"3":4}""") { js }
      assertResult(Map("1" -> 2, "3" -> 4)) {
        sj.read[Any](js)
      }
    }
    it("Map works (String,Int)") {
      val inst: Any = Map("yes" -> 1, "no" -> 2)
      val js = sj.render(inst)
      assertResult("""{"yes":1,"no":2}""") { js }
      assertResult(Map("yes" -> 1, "no" -> 2)) {
        sj.read[Any](js)
      }
    }
    /*
    it("Map works (Class,Int)") {
      val inst: Any = Map(Player("Mike", 34) -> 1, Player("Sarah", 29) -> 2)
      val js = sj.render(inst)
      assertResult("""{"{\"_hint\":\"co.blocke.scalajack.json.test.collections.Player\",\"name\":\"Mike\",\"age\":34}":1,"{\"_hint\":\"co.blocke.scalajack.json.test.collections.Player\",\"name\":\"Sarah\",\"age\":29}":2}""") { js }
      assertResult(Map(Player("Mike", 34) -> 1, Player("Sarah", 29) -> 2)) {
        sj.read[Any](js)
      }
    }
    */
  }
}
