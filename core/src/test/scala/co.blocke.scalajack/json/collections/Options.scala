package co.blocke.scalajack
package json.test.collections

import org.scalatest.{ FunSpec, Matchers }

class Options() extends FunSpec with Matchers {

  val sj = ScalaJack()

  describe("------------------\n:  Option Tests  :\n------------------") {
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

      val inst2: Option[List[Int]] = None
      val js2 = sj.render(inst2)
      assertResult("") { js2 }
      // Can't read nothing into something

      val inst3: Map[Option[List[Int]], Int] = Map(None -> 2, Some(List(1, 2, 3)) -> 1)
      val js3 = sj.render(inst3)
      assertResult("""{"[1,2,3]":1}""") { js3 }
      assertResult(Map(Some(List(1, 2, 3)) -> 1)) {
        sj.read[Map[Option[List[Int]], Int]](js3)
      }
    }
    it("Option of Map") {
      val inst: Option[Map[String, Boolean]] = Some(Map("hey" -> true, "you" -> false))
      val js = sj.render(inst)
      assertResult("""{"hey":true,"you":false}""") { js }
      assertResult(inst) {
        sj.read[Option[Map[String, Boolean]]](js)
      }

      val inst2: Option[Map[String, Boolean]] = None
      val js2 = sj.render(inst2)
      assertResult("") { js2 }
      // Can't read nothing into something

      val inst3: Map[Option[Map[String, Boolean]], Int] = Map(None -> 2, Some(Map("hey" -> true, "you" -> false)) -> 1)
      val js3 = sj.render(inst3)
      assertResult("""{"{\"hey\":true,\"you\":false}":1}""") { js3 }
      assertResult(Map(Some(Map("hey" -> true, "you" -> false)) -> 1)) {
        sj.read[Map[Option[Map[String, Boolean]], Int]](js3)
      }
    }
    it("Option of Tuple") {
      val inst: Option[(String, Boolean)] = Some(("a", true))
      val js = sj.render(inst)
      assertResult("""["a",true]""") { js }
      assertResult(inst) {
        sj.read[Option[(String, Boolean)]](js)
      }

      val inst2: Option[(String, Boolean)] = None
      val js2 = sj.render(inst2)
      assertResult("") { js2 }
      // Can't read nothing into something

      val inst3: Map[Option[(String, Boolean)], Int] = Map(None -> 2, Some(("a", true)) -> 1)
      val js3 = sj.render(inst3)
      assertResult("""{"[\"a\",true]":1}""") { js3 }
      assertResult(Map(Some(("a", true)) -> 1)) {
        sj.read[Map[Option[(String, Boolean)], Int]](js3)
      }
    }
    it("Option of Case Class") {
      val inst: Option[SomeClass] = Some(SomeClass("Mike", 2))
      val js = sj.render(inst)
      assertResult("""{"name":"Mike","age":2}""") { js }
      assertResult(inst) {
        sj.read[Option[SomeClass]](js)
      }

      val inst2: Option[SomeClass] = None
      val js2 = sj.render(inst2)
      assertResult("") { js2 }
      // Can't read nothing into something

      val inst3: Map[Option[SomeClass], Int] = Map(None -> 2, Some(SomeClass("Mike", 2)) -> 1)
      val js3 = sj.render(inst3)
      assertResult("""{"{\"name\":\"Mike\",\"age\":2}":1}""") { js3 }
      assertResult(Map(Some(SomeClass("Mike", 2)) -> 1)) {
        sj.read[Map[Option[SomeClass], Int]](js3)
      }
    }
    it("Option of Trait") {
      val inst: Option[Person] = Some(SomeClass("Mike", 2))
      val js = sj.render(inst)
      assertResult("""{"_hint":"co.blocke.scalajack.json.test.collections.SomeClass","name":"Mike","age":2}""") { js }
      assertResult(inst) {
        sj.read[Option[Person]](js)
      }

      val inst2: Option[Person] = None
      val js2 = sj.render(inst2)
      assertResult("") { js2 }
      // Can't read nothing into something

      val inst3: Map[Option[Person], Int] = Map(None -> 2, Some(SomeClass("Mike", 2)) -> 1)
      val js3 = sj.render(inst3)
      assertResult("""{"{\"_hint\":\"co.blocke.scalajack.json.test.collections.SomeClass\",\"name\":\"Mike\",\"age\":2}":1}""") { js3 }
      assertResult(Map(Some(SomeClass("Mike", 2)) -> 1)) {
        sj.read[Map[Option[Person], Int]](js3)
      }
    }
    it("Option of Parameterized Class") {
      val inst: Option[AThing[Int, String]] = Some(AThing("wow", 5))
      val js = sj.render(inst)
      assertResult("""{"a":"wow","b":5}""") { js }
      assertResult(inst) {
        sj.read[Option[AThing[Int, String]]](js)
      }

      val inst2: Option[AThing[Int, String]] = None
      val js2 = sj.render(inst2)
      assertResult("") { js2 }

      val inst3: Map[Option[AThing[Int, String]], Int] = Map(None -> 2, Some(AThing("wow", 5)) -> 1)
      val js3 = sj.render(inst3)
      assertResult("""{"{\"a\":\"wow\",\"b\":5}":1}""") { js3 }
      assertResult(Map(Some(AThing("wow", 5)) -> 1)) {
        sj.read[Map[Option[AThing[Int, String]], Int]](js3)
      }
    }
    it("Option of Parameterized Trait") {
      val inst: Option[Thing[String, Int]] = Some(AThing("wow", 5))
      val js = sj.render(inst)
      assertResult("""{"_hint":"co.blocke.scalajack.json.test.collections.AThing","a":"wow","b":5}""") { js }
      assertResult(inst) {
        sj.read[Option[Thing[String, Int]]](js)
      }

      val inst2: Option[Thing[String, Int]] = None
      val js2 = sj.render(inst2)
      assertResult("") { js2 }

      val inst3: Map[Option[Thing[String, Int]], Int] = Map(None -> 2, Some(AThing("wow", 5)) -> 1)
      val js3 = sj.render(inst3)
      assertResult("""{"{\"_hint\":\"co.blocke.scalajack.json.test.collections.AThing\",\"a\":\"wow\",\"b\":5}":1}""") { js3 }
      assertResult(Map(Some(AThing("wow", 5)) -> 1)) {
        sj.read[Map[Option[Thing[String, Int]], Int]](js3)
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
      assertResult("""{"one":1,"three":3}""") { js }
      assertResult(Map(Some("one") -> 1, Some("three") -> 3)) {
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
      assertResult(null) { sj.read[Option[Int]](js) }
    }
    it("Reading null into optional class field") {
      val js = """{"name":"Mike","age":null}"""
      assertResult(OptionClass("Mike", null)) { sj.read[OptionClass](js) }
    }
    it("Reading null into optional List item") {
      val js = """[1,null,2]"""
      assertResult(List(Some(1), null, Some(2))) { sj.read[List[Option[Int]]](js) }
    }
    it("Reading null into optional Map item") {
      val js = """{"1":"one","2":null,"3":"three"}"""
      assertResult(Map(1 -> Some("one"), 2 -> null, 3 -> Some("three"))) { sj.read[Map[Int, Option[String]]](js) }
    }
    it("Reading null into optional Tuple item") {
      val js = """[{"foo":1,"t":[true,"ok",2]},{"foo":5,"t":[false,null,3]}]"""
      assertResult(List(OptionTuple(1, (true, Some("ok"), 2)), OptionTuple(5, (false, None, 3)))) { sj.read[List[OptionTuple]](js) }
    }
  }
}
