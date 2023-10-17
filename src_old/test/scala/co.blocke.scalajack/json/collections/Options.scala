package co.blocke.scalajack
package json.collections

import co.blocke.scala_reflection._
import scala.math._
import TestUtil._
import munit._
import munit.internal.console
import co.blocke.scalajack.json.JSON
import scala.collection.immutable._
import scala.jdk.CollectionConverters._
import scala.language.implicitConversions
import java.util.Optional
import json.JsonMatcher


class Options() extends FunSuite:

  val sj = co.blocke.scalajack.ScalaJack()
  
  test("Option of primitive (naked)") {
    describe("--------------------------------------------\n:  Option (Scala) & Optional (Java) Tests  :\n--------------------------------------------", Console.BLUE)
    describe("+++ Scala Options")

    val inst: Option[BigInt] = Some(BigInt(5))
    val js = sj.render(inst)
    assertEquals("5".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[Option[BigInt]](js))
  }

  test("Option of primitive (in class)") {
    val inst = OptionBigInt(Some(BigInt(5)))
    val js = sj.render(inst)
    assertEquals("""{"o":5}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[OptionBigInt](js))
  }

  test("Option of List") {
    val inst: Option[List[Int]] = Some(List(1, 2, 3))
    val js = sj.render(inst)
    assertEquals("""[1,2,3]""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[Option[List[Int]]](js))

    val inst2: Option[List[Int]] = None
    val js2 = sj.render(inst2)
    assertEquals("".asInstanceOf[JSON],js2)
    // Can't read nothing into something

    val inst3: Map[Option[List[Int]], Int] =
      Map(None -> 2, Some(List(1, 2, 3)) -> 1)
    val js3 = sj.render(inst3)
    assertEquals("""{"[1,2,3]":1}""".asInstanceOf[JSON],js3)
    assertEquals(Map(Some(List(1, 2, 3)) -> 1), sj.read[Map[Option[List[Int]], Int]](js3))
  }

  test("Option of Map") {
    val inst: Option[Map[String, Boolean]] =
      Some(Map("hey" -> true, "you" -> false))
    val js = sj.render(inst)
    assertEquals("""{"hey":true,"you":false}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[Option[Map[String, Boolean]]](js))

    val inst2: Option[Map[String, Boolean]] = None
    val js2 = sj.render(inst2)
    assertEquals("".asInstanceOf[JSON],js2)
    // Can't read nothing into something

    val inst3: Map[Option[Map[String, Boolean]], Int] =
      Map(None -> 2, Some(Map("hey" -> true, "you" -> false)) -> 1)
    val js3 = sj.render(inst3)
    assertEquals("""{"{\"hey\":true,\"you\":false}":1}""".asInstanceOf[JSON],js3)
    assertEquals(Map(Some(Map("hey" -> true, "you" -> false)) -> 1), sj.read[Map[Option[Map[String, Boolean]], Int]](js3))
  }

  test("Option of Tuple") {
    val inst: Option[(String, Boolean)] = Some(("a", true))
    val js = sj.render(inst)
    assertEquals("""["a",true]""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[Option[(String, Boolean)]](js))

    val inst2: Option[(String, Boolean)] = None
    val js2 = sj.render(inst2)
    assertEquals("".asInstanceOf[JSON],js2)
    // Can't read nothing into something

    val inst3: Map[Option[(String, Boolean)], Int] =
      Map(None -> 2, Some(("a", true)) -> 1)
    val js3 = sj.render(inst3)
    assertEquals("""{"[\"a\",true]":1}""".asInstanceOf[JSON],js3)
    assertEquals(Map(Some(("a", true)) -> 1), sj.read[Map[Option[(String, Boolean)], Int]](js3))
  }

  test("Option of Case Class") {
    val inst: Option[SomeClass] = Some(SomeClass("Mike", 2))
    val js = sj.render(inst)
    assertEquals("""{"name":"Mike","age":2}""".asInstanceOf[JSON],js)
    assertEquals(inst,sj.read[Option[SomeClass]](js))

    val inst2: Option[SomeClass] = None
    val js2 = sj.render(inst2)
    assertEquals("".asInstanceOf[JSON],js2)
    // Can't read nothing into something

    val inst3: Map[Option[SomeClass], Int] = Map(None -> 2, Some(SomeClass("Mike", 2)) -> 1)
    val js3 = sj.render(inst3)
    assertEquals("""{"{\"name\":\"Mike\",\"age\":2}":1}""".asInstanceOf[JSON],js3)
    assertEquals(Map(Some(SomeClass("Mike", 2)) -> 1),sj.read[Map[Option[SomeClass], Int]](js3))
  }

  test("Option of Parameterized Class") {
    val inst: Option[AThing[Int, String]] = Some(AThing("wow", 5))
    val js = sj.render(inst)
    assertEquals("""{"a":"wow","b":5}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[Option[AThing[Int, String]]](js))

    val inst2: Option[AThing[Int, String]] = None
    val js2 = sj.render(inst2)
    assertEquals("".asInstanceOf[JSON],js2)

    val inst3: Map[Option[AThing[Int, String]], Int] = Map(None -> 2, Some(AThing("wow", 5)) -> 1)
    val js3 = sj.render(inst3)
    assertEquals("""{"{\"a\":\"wow\",\"b\":5}":1}""".asInstanceOf[JSON],js3)
    assertEquals(Map(Some(AThing("wow", 5)) -> 1), sj.read[Map[Option[AThing[Int, String]], Int]](js3))
  }

  test("Option of Trait") {
    val inst: Option[Person] = Some(SomeClass("Mike", 2))
    val js = sj.render(inst)
    assertEquals(
      """{"_hint":"co.blocke.scalajack.json.collections.SomeClass","name":"Mike","age":2}""".asInstanceOf[JSON],js)
    assertEquals(inst,sj.read[Option[Person]](js))

    val inst2: Option[Person] = None
    val js2 = sj.render(inst2)
    assertEquals("".asInstanceOf[JSON],js2)
    // Can't read nothing into something

    val inst3: Map[Option[Person], Int] =
      Map(None -> 2, Some(SomeClass("Mike", 2)) -> 1)
    val js3 = sj.render(inst3)
    assertEquals(
      """{"{\"_hint\":\"co.blocke.scalajack.json.collections.SomeClass\",\"name\":\"Mike\",\"age\":2}":1}""".asInstanceOf[JSON],js3)
    assertEquals(Map(Some(SomeClass("Mike", 2)) -> 1),sj.read[Map[Option[Person], Int]](js3))
  }

  test("Option of Parameterized Trait") {
    val inst: Option[Thing[String, Int]] = Some(AThing("wow", 5))
    val js = sj.render(inst)
    assertEquals(
      """{"_hint":"co.blocke.scalajack.json.collections.AThing","a":"wow","b":5}""".asInstanceOf[JSON],js)
    assertEquals(inst,sj.read[Option[Thing[String, Int]]](js))

    val inst2: Option[Thing[String, Int]] = None
    val js2 = sj.render(inst2)
    assertEquals("".asInstanceOf[JSON],js2)

    val inst3: Map[Option[Thing[String, Int]], Int] =
      Map(None -> 2, Some(AThing("wow", 5)) -> 1)
    val js3 = sj.render(inst3)
    assertEquals(
      """{"{\"_hint\":\"co.blocke.scalajack.json.collections.AThing\",\"a\":\"wow\",\"b\":5}":1}""".asInstanceOf[JSON],js3)
    assertEquals(Map(Some(AThing("wow", 5)) -> 1),sj.read[Map[Option[Thing[String, Int]], Int]](js3))
  }

  //------------------- None tests
  test("Option is None (in class)") {
    describe("+++ Scala None/null tests")

    val inst = OptionClass("Mike", None)
    val js = sj.render(inst)
    assertEquals("""{"name":"Mike"}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[OptionClass](js))
  }

  test("Option is None (in List)") {
    val inst: List[Option[Int]] = List(Some(1), None, Some(2))
    val js = sj.render(inst)
    assertEquals("""[1,2]""".asInstanceOf[JSON],js)
    assertEquals(List(Some(1), Some(2)).asInstanceOf[List[Option[Int]]], sj.read[List[Option[Int]]](js))  // None gets erased here
  }

  test("Option is None (value in Map)") {
    val inst: Map[Int, Option[String]] = Map(1 -> Some("one"), 2 -> None, 3 -> Some("three"))
    val js = sj.render(inst)
    assertEquals("""{"1":"one","3":"three"}""".asInstanceOf[JSON],js)
    assertEquals(Map(1 -> Some("one"), 3 -> Some("three")).asInstanceOf[Map[Int,Option[String]]], sj.read[Map[Int, Option[String]]](js)) // None gets erased here
  }

  test("Option is None (key in Map)") {
    val inst: Map[Option[String], Int] =
      Map(Some("one") -> 1, None -> 2, Some("three") -> 3)
    val js = sj.render(inst)
    assertEquals("""{"one":1,"three":3}""".asInstanceOf[JSON],js)
    assertEquals(Map(Some("one") -> 1, Some("three") -> 3),sj.read[Map[Option[String], Int]](js))
  }

  test("Option is None (in Tuple)") {
    val inst = List(
      OptionTuple(1, (true, Some("ok"), 2)),
      OptionTuple(5, (false, None, 3))
    )
    val js = sj.render(inst)
    assertEquals(
      """[{"foo":1,"t":[true,"ok",2]},{"foo":5,"t":[false,null,3]}]""".asInstanceOf[JSON],js)
    assertEquals(inst,sj.read[List[OptionTuple]](js))
  }

  test("Reading null into optional (naked)") {
    val js = "null".asInstanceOf[JSON]
    assertEquals(null.asInstanceOf[Option[Int]], sj.read[Option[Int]](js))
  }

  test("Reading null into optional class field") {
    val js = """{"name":"Mike","age":null}""".asInstanceOf[JSON]
    assertEquals(OptionClass("Mike", null), sj.read[OptionClass](js))
  }

  test("Reading null into optional List item") {
    val js = """[1,null,2]""".asInstanceOf[JSON]
    assertEquals(List(Some(1), null, Some(2)).asInstanceOf[List[Option[Int]]],sj.read[List[Option[Int]]](js))
  }
  
  test("Reading null into optional Map item") {
    val js = """{"1":"one","2":null,"3":"three"}""".asInstanceOf[JSON]
    assertEquals(Map(1 -> Some("one"), 2 -> None, 3 -> Some("three")),sj.read[Map[Int, Option[String]]](js))
  }

  test("Reading null into optional Tuple item") {
    val js = """[{"foo":1,"t":[true,"ok",2]},{"foo":5,"t":[false,null,3]}]""".asInstanceOf[JSON]
    assertEquals(
      List(
        OptionTuple(1, (true, Some("ok"), 2)),
        OptionTuple(5, (false, None, 3))
      ),sj.read[List[OptionTuple]](js))
  }

  //-------------- Java
  
  test("Option of primitive (naked)") {
    describe("++++ Java Optional")

    val inst: Optional[BigInt] = Optional.of(BigInt(5))
    val js = sj.render(inst)
    assertEquals("5".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[Optional[BigInt]](js))
  }

  test("Java class with Optional - empty JSON") {
    val js = """{}""".asInstanceOf[JSON]
    val inst = sj.read[Maybe](js)
    assertEquals(inst.getOne, Optional.empty)
    assertEquals(inst.getTwo, Optional.of("stuff"))
    assertEquals( """{"two":"stuff"}""".asInstanceOf[JSON], sj.render(inst))

    val msg = """Class co.blocke.scalajack.Maybe2 missing required fields: one
      |{}
      |-^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[Maybe2](js)
    }
  }

  test("Java class with Optional - some args specified") {
    val js = """{"one":"meh"}""".asInstanceOf[JSON]
    val inst = sj.read[Maybe](js)
    assertEquals(inst.getOne, Optional.of("meh"))
    assertEquals(inst.getTwo, Optional.of("stuff"))
    assert(JsonMatcher.jsonMatches("""{"one":"meh","two":"stuff"}""".asInstanceOf[JSON], sj.render(inst)))
  }

  test("Optional of primitive (in class)") {
    val inst = OptionalBigInt(Optional.of(BigInt(5)))
    val js = sj.render(inst)
    assertEquals("""{"o":5}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[OptionalBigInt](js))
  }

  test("Optional of List") {
    val inst: Optional[List[Int]] = Optional.of(List(1, 2, 3))
    val js = sj.render(inst)
    assertEquals("""[1,2,3]""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[Optional[List[Int]]](js))

    val inst2: Optional[List[Int]] = Optional.empty[List[Int]]
    val js2 = sj.render(inst2)
    assertEquals("".asInstanceOf[JSON],js2)
    // Can't read nothing into something

    val inst3 = Map(Optional.empty[List[Int]] -> 2, Optional.of(List(1, 2, 3)) -> 1)
    val js3 = sj.render(inst3)
    assertEquals("""{"[1,2,3]":1}""".asInstanceOf[JSON],js3)
    assert(Map(Optional.of(List(1, 2, 3)) -> 1) == sj.read[Map[Optional[List[Int]], Int]](js3))
  }

  test("Optional of Map") {
    val inst: Optional[Map[String, Boolean]] =
      Optional.of(Map("hey" -> true, "you" -> false))
    val js = sj.render(inst)
    assertEquals("""{"hey":true,"you":false}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[Optional[Map[String, Boolean]]](js))

    val inst2: Option[Map[String, Boolean]] = None
    val js2 = sj.render(inst2)
    assertEquals("".asInstanceOf[JSON],js2)
    // Can't read nothing into something

    val inst3: Map[Option[Map[String, Boolean]], Int] =
      Map(None -> 2, Some(Map("hey" -> true, "you" -> false)) -> 1)
    val js3 = sj.render(inst3)
    assertEquals("""{"{\"hey\":true,\"you\":false}":1}""".asInstanceOf[JSON],js3)
    assertEquals(Map(Some(Map("hey" -> true, "you" -> false)) -> 1), sj.read[Map[Option[Map[String, Boolean]], Int]](js3))
  }

  test("Optional of Tuple") {
    val inst: Optional[(String, Boolean)] = Optional.of(("a", true))
    val js = sj.render(inst)
    assertEquals("""["a",true]""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[Optional[(String, Boolean)]](js))

    val inst2: Optional[(String, Boolean)] = Optional.empty[(String,Boolean)]
    val js2 = sj.render(inst2)
    assertEquals("".asInstanceOf[JSON],js2)
    // Can't read nothing into something

    val inst3: Map[Optional[(String, Boolean)], Int] =
      Map(Optional.empty[(String, Boolean)] -> 2, Optional.of(("a", true)) -> 1)
    val js3 = sj.render(inst3)
    assertEquals("""{"[\"a\",true]":1}""".asInstanceOf[JSON],js3)
    assertEquals(Map(Optional.of(("a", true)) -> 1), sj.read[Map[Optional[(String, Boolean)], Int]](js3))
  }

  test("Optional of Case Class") {
    val inst: Optional[SomeClass] = Optional.of(SomeClass("Mike", 2))
    val js = sj.render(inst)
    assertEquals("""{"name":"Mike","age":2}""".asInstanceOf[JSON],js)
    assertEquals(inst,sj.read[Optional[SomeClass]](js))

    val inst2: Optional[SomeClass] = Optional.empty[SomeClass]
    val js2 = sj.render(inst2)
    assertEquals("".asInstanceOf[JSON],js2)
    // Can't read nothing into something

    val inst3: Map[Optional[SomeClass], Int] = Map(Optional.empty[SomeClass] -> 2, Optional.of(SomeClass("Mike", 2)) -> 1)
    val js3 = sj.render(inst3)
    assertEquals("""{"{\"name\":\"Mike\",\"age\":2}":1}""".asInstanceOf[JSON],js3)
    assertEquals(Map(Optional.of(SomeClass("Mike", 2)) -> 1),sj.read[Map[Optional[SomeClass], Int]](js3))
  }

  test("Optional of Parameterized Class") {
    val inst: Optional[AThing[Int, String]] = Optional.of(AThing("wow", 5))
    val js = sj.render(inst)
    assertEquals("""{"a":"wow","b":5}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[Optional[AThing[Int, String]]](js))

    val inst2: Optional[AThing[Int, String]] = Optional.empty[AThing[Int, String]]
    val js2 = sj.render(inst2)
    assertEquals("".asInstanceOf[JSON],js2)

    val inst3: Map[Optional[AThing[Int, String]], Int] =
      Map(Optional.empty[AThing[Int, String]] -> 2, Optional.of(AThing("wow", 5)) -> 1)
    val js3 = sj.render(inst3)
    assertEquals("""{"{\"a\":\"wow\",\"b\":5}":1}""".asInstanceOf[JSON],js3)
    assertEquals(Map(Optional.of(AThing("wow", 5)) -> 1), sj.read[Map[Optional[AThing[Int, String]], Int]](js3))
  }

  test("Optional is None (in class)") {
    describe("+++ Java Empty/null tests")

    val inst = OptionalClass("Mike", Optional.empty[Int])
    val js = sj.render(inst)
    assertEquals("""{"name":"Mike"}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[OptionalClass](js))
  }

  test("Optional is None (in List)") {
    val inst: List[Optional[Int]] = List(Optional.of(1), Optional.empty[Int], Optional.of(2))
    val js = sj.render(inst)
    assertEquals("""[1,2]""".asInstanceOf[JSON],js)
    assertEquals(List(Optional.of(1), Optional.of(2)).asInstanceOf[List[Optional[Int]]], sj.read[List[Optional[Int]]](js))  // None gets erased here
  }

  test("Optional is None (value in Map)") {
    val inst: Map[Int, Optional[String]] = Map(1 -> Optional.of("one"), 2 -> Optional.empty[String], 3 -> Optional.of("three"))
    val js = sj.render(inst)
    assertEquals("""{"1":"one","3":"three"}""".asInstanceOf[JSON],js)
    assert(Map(1 -> Optional.of("one"), 3 -> Optional.of("three")).asInstanceOf[Map[Int,Option[String]]] == sj.read[Map[Int, Optional[String]]](js)) // None gets erased here
  }

  test("Optional is None (key in Map)") {
    val inst: Map[Optional[String], Int] =
      Map(Optional.of("one") -> 1, Optional.empty[String] -> 2, Optional.of("three") -> 3)
    val js = sj.render(inst)
    assertEquals("""{"one":1,"three":3}""".asInstanceOf[JSON],js)
    assertEquals(Map(Optional.of("one") -> 1, Optional.of("three") -> 3),sj.read[Map[Optional[String], Int]](js))
  }

  test("Optional is Empty (in Tuple)") {
    val inst = List(
      OptionalTuple(1, (true, Optional.of("ok"), 2)),
      OptionalTuple(5, (false, Optional.empty[String], 3))
    )
    val js = sj.render(inst)
    assertEquals(
      """[{"foo":1,"t":[true,"ok",2]},{"foo":5,"t":[false,null,3]}]""".asInstanceOf[JSON],js)
    assertEquals(inst,sj.read[List[OptionalTuple]](js))
  }

  test("Reading null into Optional (naked)") {
    val js = "null".asInstanceOf[JSON]
    assertEquals(null.asInstanceOf[Optional[Int]], sj.read[Optional[Int]](js))
  }

  test("Reading null into Optional class field") {
    val js = """{"name":"Mike","age":null}""".asInstanceOf[JSON]
    assertEquals(OptionalClass("Mike", null), sj.read[OptionalClass](js))
  }

  test("Reading null into Optional List item") {
    val js = """[1,null,2]""".asInstanceOf[JSON]
    assertEquals(List(Optional.of(1), null, Optional.of(2)).asInstanceOf[List[Optional[Int]]],sj.read[List[Optional[Int]]](js))
  }

  test("Reading null into Optional Map item") {
    val js = """{"1":"one","2":null,"3":"three"}""".asInstanceOf[JSON]
    assertEquals(Map(1 -> Optional.of("one"), 2 -> Optional.empty[String], 3 -> Optional.of("three")),sj.read[Map[Int, Optional[String]]](js))
  }

  test("Reading null into Optional Tuple item") {
    val js = """[{"foo":1,"t":[true,"ok",2]},{"foo":5,"t":[false,null,3]}]""".asInstanceOf[JSON]
    assertEquals(
      List(
        OptionalTuple(1, (true, Optional.of("ok"), 2)),
        OptionalTuple(5, (false, Optional.empty[String], 3))
      ),sj.read[List[OptionalTuple]](js))
  }
