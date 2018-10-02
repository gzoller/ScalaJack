package co.blocke.scalajack
package json.test.collections

import org.scalatest.{ FunSpec, Matchers }

class Options() extends FunSpec with Matchers {

  val sj = ScalaJack()

  describe("------------------\n:  Option Tests  :\n------------------") {
    /*
    it("Option of primitive (naked)") {
      val inst: Option[BigInt] = Some(BigInt(5))
      val js = sj.render(inst)
      assertResult("5") { js }
      assertResult(inst) {
        sj.read[Option[BigInt]](js)
      }
    }
    it("Option of primitive (in class)") {
      val inst = OptionBigInt(Some(BigInt(5)))
      val js = sj.render(inst)
      assertResult("""{"o":5}""") { js }
      assertResult(inst) {
        sj.read[OptionBigInt](js)
      }
    }
    it("Option of List") {
      val inst: Option[List[Int]] = Some(List(1, 2, 3))
      val js = sj.render(inst)
      assertResult("""[1,2,3]""") { js }
      assertResult(inst) {
        sj.read[Option[List[Int]]](js)
      }
    }
    it("Option of Map") {
      val inst: Option[Map[String, Boolean]] = Some(Map("hey" -> true, "you" -> false))
      val js = sj.render(inst)
      assertResult("""{"hey":true,"you":false}""") { js }
      assertResult(inst) {
        sj.read[Option[Map[String, Boolean]]](js)
      }
    }
    it("Option of Tuple") {
      val inst: Option[(String, Boolean)] = Some(("a", true))
      val js = sj.render(inst)
      assertResult("""["a",true]""") { js }
      assertResult(inst) {
        sj.read[Option[(String, Boolean)]](js)
      }
    }
    it("Option of Case Class") {
      val inst: Option[SomeClass] = Some(SomeClass("Mike", 2))
      val js = sj.render(inst)
      assertResult("""{"name":"Mike","age":2}""") { js }
      assertResult(inst) {
        sj.read[Option[SomeClass]](js)
      }
    }
    it("Option of Trait") {
      val inst: Option[Person] = Some(SomeClass("Mike", 2))
      val js = sj.render(inst)
      assertResult("""{"_hint":"co.blocke.scalajack.json.test.collections.SomeClass","name":"Mike","age":2}""") { js }
      assertResult(inst) {
        sj.read[Option[Person]](js)
      }
    }
    it("Option of Parameterized Class") {
      val inst: Option[AThing[Int, String]] = Some(AThing("wow", 5))
      val js = sj.render(inst)
      assertResult("""{"a":"wow","b":5}""") { js }
      assertResult(inst) {
        sj.read[Option[AThing[Int, String]]](js)
      }
    }
    it("Option of Parameterized Trait") {
      val inst: Option[Thing[String, Int]] = Some(AThing("wow", 5))
      val js = sj.render(inst)
      assertResult("""{"_hint":"co.blocke.scalajack.json.test.collections.AThing","a":"wow","b":5}""") { js }
      assertResult(inst) {
        sj.read[Option[Thing[String, Int]]](js)
      }
    }
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
      assertResult("""[1,null,2]""") { js }
      assertResult(inst) {
        sj.read[List[Option[Int]]](js)
      }
    }
    */
    it("Option is None (value in Map)") {
      val inst: Map[Int, Option[String]] = Map(1 -> Some("one"), 2 -> None, 3 -> Some("three"))
      val js = sj.render(inst)
      println("JS: "+js)
      assertResult("""{"1":"one","3":"three"}""") { js }
      assertResult(Map(1 -> Some("one"), 3 -> Some("three"))) { // The None gets erased here
        sj.read[Map[Int, Option[String]]](js)
      }
    }
    /*
    it("Option is None (key in Map)") {
      val inst: Map[Option[String], Int] = Map(Some("one") -> 1, None -> 2, Some("three") -> 3)
      val js = sj.render(inst)
      assertResult("""{"one":1,"":2,"three":3}""") { js }
      assertResult(inst) {
        sj.read[Map[Option[String], Int]](js)
      }
    }
    it("Option is None (in Tuple)") {
      val inst = List(OptionTuple(1, (true, Some("ok"), 2)), OptionTuple(5, (false, None, 3)))
      val js = sj.render(inst)
      assertResult("""[{"foo":1,"t":[true,"ok",2]},{"foo":5,"t":[false,null,3]}]""") { js }
      assertResult(inst) {
        sj.read[List[OptionTuple]](js)
      }
    }
    it("Reading null into optional (naked)") {
      val js = "null"
      assertResult(None) { sj.read[Option[Int]](js) }
    }
    it("Reading null into optional class field") {
      val js = """{"name":"Mike","age":null}"""
      assertResult(OptionClass("Mike", None)) { sj.read[OptionClass](js) }
    }
    it("Reading null into optional List item") {
      val js = """[1,null,2]"""
      assertResult(List(Some(1), None, Some(2))) { sj.read[List[Option[Int]]](js) }
    }
    it("Reading null into optional Map item") {
      val js = """{"1":"one","2":null,"3":"three"}"""
      assertResult(Map(1 -> Some("one"), 2 -> None, 3 -> Some("three"))) { sj.read[Map[Int, Option[String]]](js) }
    }
    it("Reading null into optional Tuple item") {
      val js = """[{"foo":1,"t":[true,"ok",2]},{"foo":5,"t":[false,null,3]}]"""
      assertResult(List(OptionTuple(1, (true, Some("ok"), 2)), OptionTuple(5, (false, None, 3)))) { sj.read[List[OptionTuple]](js) }
    }
    */
  }
}
