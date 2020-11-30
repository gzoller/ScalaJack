package co.blocke.scalajack
package json.mapkeys

import co.blocke.scala_reflection._
import TestUtil._
import munit._
import munit.internal.console
import co.blocke.scalajack.json.JSON

class ListCollKeys() extends FunSuite:

  val sj = co.blocke.scalajack.ScalaJack()

  test("List as key") {
    describe(
      "------------------------\n:  List Map Key Tests  :\n------------------------", Console.BLUE
    )
    
    val l1 = List(1, 2, 3)
    val l2 = List(4, 5, 6)
    val inst = Map(l1 -> l2)
    val js = sj.render(inst)
    assertEquals("""{"[1,2,3]":[4,5,6]}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[Map[List[Int], List[Int]]](js))
  }

  test("List of Lists as key") {
    val l1 = List(List(1, 2, 3), List(9, 8, 7))
    val l2 = List(List(4, 5, 6), List(1, 3, 5))
    val inst = Map(l1 -> l2)
    val js = sj.render(inst)
    assertEquals("""{"[[1,2,3],[9,8,7]]":[[4,5,6],[1,3,5]]}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[Map[List[List[Int]], List[List[Int]]]](js))
  }

  test("List of Tuples as key") {
    val l1: List[(String, String)] = List(("A", "a"), ("B", "b"), (null, "c"))
    val l2: List[(String, String)] = List(("X", "x"), ("Y", "y"), (null, "z"))
    val inst = Map(l1 -> l2)
    val js = sj.render(inst)
    assertEquals(
      """{"[[\"A\",\"a\"],[\"B\",\"b\"],[null,\"c\"]]":[["X","x"],["Y","y"],[null,"z"]]}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[Map[List[(String, String)], List[(String, String)]]](js))
  }

  test("List of Maps as key") {
    val l1 = List(Map("wow" -> true), Map("ya" -> false))
    val l2 = List(Map("zing" -> false), Map("bling" -> true))
    val inst = Map(l1 -> l2)
    val js = sj.render(inst)
    assertEquals(
      """{"[{\"wow\":true},{\"ya\":false}]":[{"zing":false},{"bling":true}]}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[Map[List[Map[String, Boolean]], List[Map[String, Boolean]]]](js))
  }

  test("List of Case Class as key") {
    val fish = FishPet("Flipper", Food.Meat, 68.9)
    val inst = Map(List(fish, fish) -> List(fish, fish))
    val js = sj.render(inst)
    assertEquals(
      """{"[{\"name\":\"Flipper\",\"food\":\"Meat\",\"waterTemp\":68.9},{\"name\":\"Flipper\",\"food\":\"Meat\",\"waterTemp\":68.9}]":[{"name":"Flipper","food":"Meat","waterTemp":68.9},{"name":"Flipper","food":"Meat","waterTemp":68.9}]}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[Map[List[FishPet], List[FishPet]]](js))
  }

  test("List of Trait as key") {
    val fish: Pet = FishPet("Flipper", Food.Meat, 68.9)
    val inst = Map(List(fish, fish) -> List(fish, fish))
    val js = sj.render(inst)
    assertEquals(
      """{"[{\"_hint\":\"co.blocke.scalajack.json.mapkeys.FishPet\",\"name\":\"Flipper\",\"food\":\"Meat\",\"waterTemp\":68.9},{\"_hint\":\"co.blocke.scalajack.json.mapkeys.FishPet\",\"name\":\"Flipper\",\"food\":\"Meat\",\"waterTemp\":68.9}]":[{"_hint":"co.blocke.scalajack.json.mapkeys.FishPet","name":"Flipper","food":"Meat","waterTemp":68.9},{"_hint":"co.blocke.scalajack.json.mapkeys.FishPet","name":"Flipper","food":"Meat","waterTemp":68.9}]}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[Map[List[Pet], List[Pet]]](js))
  }

  test("List of Any as key") {
    val inst: Map[List[Any], List[Any]] =
      Map(List(23L, "wow", true) -> List(12.2, 0))
    val js = sj.render(inst)
    assertEquals("""{"[23,\"wow\",true]":[12.2,0.0]}""".asInstanceOf[JSON],js)
    assertEquals(true, sj.read[Map[List[Any], List[Any]]](js).isInstanceOf[Map[List[Any], List[Any]]])
  }

  test("List of parameterized class as key") {
    val inst = Map(
      List(AThing(true, "True"), AThing(false, "False")) -> List(
        AThing(true, "Yes"),
        AThing(false, "No")
      )
    )
    val js = sj.render(inst)
    assertEquals(
      """{"[{\"a\":true,\"b\":\"True\"},{\"a\":false,\"b\":\"False\"}]":[{"a":true,"b":"Yes"},{"a":false,"b":"No"}]}""".asInstanceOf[JSON],js)
    assertEquals(true,  sj.read[Map[List[AThing[String, Boolean]], List[AThing[String, Boolean]]]](js)
        .isInstanceOf[Map[List[AThing[String, Boolean]], List[AThing[String, Boolean]]]])
  }

  test("List of parameterized trait as key") {
    val inst: Map[List[Thing[Boolean, String]], List[Thing[Boolean, String]]] =
      Map(
        List(AThing(true, "True"), AThing(false, "False")) -> List(
          AThing(true, "Yes"),
          AThing(false, "No")
        )
      )
    val js = sj.render(inst)
    assertEquals(
      """{"[{\"_hint\":\"co.blocke.scalajack.json.mapkeys.AThing\",\"a\":true,\"b\":\"True\"},{\"_hint\":\"co.blocke.scalajack.json.mapkeys.AThing\",\"a\":false,\"b\":\"False\"}]":[{"_hint":"co.blocke.scalajack.json.mapkeys.AThing","a":true,"b":"Yes"},{"_hint":"co.blocke.scalajack.json.mapkeys.AThing","a":false,"b":"No"}]}""".asInstanceOf[JSON],js)
    assertEquals(true, sj.read[Map[List[Thing[Boolean, String]], List[Thing[Boolean, String]]]](js)
        .isInstanceOf[Map[List[Thing[Boolean, String]], List[Thing[Boolean, String]]]])
  }

  test("List of Optional as key") {
    val inst: Map[List[Option[String]], List[Option[String]]] =
      Map(List(Some("hey"), Some("you")) -> List(Some("stop"), Some("go")))
    val js = sj.render(inst)
    assertEquals("""{"[\"hey\",\"you\"]":["stop","go"]}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[Map[List[Option[String]], List[Option[String]]]](js))
  }

  test("List of ValueClass as key") {
    val inst =
      Map(List(VCChar('A'), VCChar('a')) -> List(VCChar('B'), VCChar('b')))
    val js = sj.render(inst)
    assertEquals("""{"[\"A\",\"a\"]":["B","b"]}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[Map[List[VCChar], List[VCChar]]](js))
  }
