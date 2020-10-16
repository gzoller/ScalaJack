package co.blocke.scalajack
package yaml
package mapkeys

import TestUtil._
import munit._
import munit.internal.console

class ListCollKeys() extends FunSuite:

  val sj = ScalaJack(YamlFlavor())

  test("List as key") {
    describe(
      "-------------------------------\n:  List Map Key Tests (YAML)  :\n-------------------------------", Console.BLUE
    )
    val l1         = List(1, 2, 3)
    val l2         = List(4, 5, 6)
    val inst       = Map(l1 -> l2)
    val yaml       = sj.render(inst)
    val comparison = """? [1, 2, 3]
                        |: [4, 5, 6]
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[Map[List[Int], List[Int]]](yaml))
  }

  test("List of Lists as key") {
    val l1         = List(List(1, 2, 3), List(9, 8, 7))
    val l2         = List(List(4, 5, 6), List(1, 3, 5))
    val inst       = Map(l1 -> l2)
    val yaml       = sj.render(inst)
    val comparison = """? - [1, 2, 3]
                        |  - [9, 8, 7]
                        |: - [4, 5, 6]
                        |  - [1, 3, 5]
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[Map[List[List[Int]], List[List[Int]]]](yaml))
  }

  test("List of Tuples as key") {
    val l1: List[(String, String)] = List(("A", "a"), ("B", "b"), (null, "c"))
    val l2: List[(String, String)] = List(("X", "x"), ("Y", "y"), (null, "z"))
    val inst                       = Map(l1 -> l2)
    val yaml                       = sj.render(inst)
    val comparison                 = """? - [A, a]
                        |  - [B, b]
                        |  - [null, c]
                        |: - [X, x]
                        |  - [Y, y]
                        |  - [null, z]
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[Map[List[(String, String)], List[(String, String)]]](yaml))
  }

  test("List of Maps as key") {
    val l1         = List(Map("wow" -> true), Map("ya" -> false))
    val l2         = List(Map("zing" -> false), Map("bling" -> true))
    val inst       = Map(l1 -> l2)
    val yaml       = sj.render(inst)
    val comparison = """? - wow: true
                        |  - ya: false
                        |: - zing: false
                        |  - bling: true
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[Map[List[Map[String, Boolean]], List[Map[String, Boolean]]]](yaml))
  }

  test("List of Case Class as key") {
    val fish       = FishPet("Flipper", Food.Meat, 68.9)
    val inst       = Map(List(fish, fish) -> List(fish, fish))
    val yaml       = sj.render(inst)
    val comparison = """? - name: Flipper
                        |    food: Meat
                        |    waterTemp: 68.9
                        |  - name: Flipper
                        |    food: Meat
                        |    waterTemp: 68.9
                        |: - name: Flipper
                        |    food: Meat
                        |    waterTemp: 68.9
                        |  - name: Flipper
                        |    food: Meat
                        |    waterTemp: 68.9
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[Map[List[FishPet], List[FishPet]]](yaml))
  }

  test("List of Trait as key") {
    val fish: Pet  = FishPet("Flipper", Food.Meat, 68.9)
    val inst       = Map(List(fish, fish) -> List(fish, fish))
    val yaml       = sj.render(inst)
    val comparison = """? - _hint: co.blocke.scalajack.yaml.mapkeys.FishPet
                        |    name: Flipper
                        |    food: Meat
                        |    waterTemp: 68.9
                        |  - _hint: co.blocke.scalajack.yaml.mapkeys.FishPet
                        |    name: Flipper
                        |    food: Meat
                        |    waterTemp: 68.9
                        |: - _hint: co.blocke.scalajack.yaml.mapkeys.FishPet
                        |    name: Flipper
                        |    food: Meat
                        |    waterTemp: 68.9
                        |  - _hint: co.blocke.scalajack.yaml.mapkeys.FishPet
                        |    name: Flipper
                        |    food: Meat
                        |    waterTemp: 68.9
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[Map[List[Pet], List[Pet]]](yaml))
  }

  test("List of Any as key") {
    val inst: Map[List[Any], List[Any]] =
      Map(List(23L, "wow", true) -> List(12.2, 0))
    val yaml       = sj.render(inst)
    val comparison = """? - 23
                        |  - wow
                        |  - true
                        |: - 12.2
                        |  - 0.0
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(true,
      sj.read[Map[List[Any], List[Any]]](yaml)
        .isInstanceOf[Map[List[Any], List[Any]]]
    )
  }

  test("List of parameterized class as key") {
    val inst = Map(
      List(AThing(true, "True"), AThing(false, "False")) -> List(
        AThing(true, "Yes"),
        AThing(false, "No")
      )
    )
    val yaml       = sj.render(inst)
    val comparison = """? - a: true
                        |    b: True
                        |  - a: false
                        |    b: False
                        |: - a: true
                        |    b: Yes
                        |  - a: false
                        |    b: No
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(true,
      sj.read[Map[List[AThing[String, Boolean]], List[AThing[String, Boolean]]]](yaml)
        .isInstanceOf[Map[List[AThing[String, Boolean]], List[AThing[String, Boolean]]]]
    )
  }

  test("List of parameterized trait as key") {
    val inst: Map[List[Thing[Boolean, String]], List[Thing[Boolean, String]]] =
      Map(
        List(AThing(true, "True"), AThing(false, "False")) -> List(
          AThing(true, "Yes"),
          AThing(false, "No")
        )
      )
    val yaml       = sj.render(inst)
    val comparison = """? - _hint: co.blocke.scalajack.yaml.mapkeys.AThing
                        |    a: true
                        |    b: True
                        |  - _hint: co.blocke.scalajack.yaml.mapkeys.AThing
                        |    a: false
                        |    b: False
                        |: - _hint: co.blocke.scalajack.yaml.mapkeys.AThing
                        |    a: true
                        |    b: Yes
                        |  - _hint: co.blocke.scalajack.yaml.mapkeys.AThing
                        |    a: false
                        |    b: No
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(true,
      sj.read[Map[List[Thing[Boolean, String]], List[Thing[Boolean, String]]]](yaml)
        .isInstanceOf[Map[List[Thing[Boolean, String]], List[Thing[Boolean, String]]]]
    )
  }

  test("List of Optional as key") {
    val inst: Map[List[Option[String]], List[Option[String]]] =
      Map(List(Some("hey"), Some("you")) -> List(Some("stop"), Some("go")))
    val yaml       = sj.render(inst)
    val comparison = """? - hey
                        |  - you
                        |: - stop
                        |  - go
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[Map[List[Option[String]], List[Option[String]]]](yaml))
  }

  test("List of ValueClass as key") {
    val inst =
      Map(List(VCChar('A'), VCChar('a')) -> List(VCChar('B'), VCChar('b')))
    val yaml       = sj.render(inst)
    val comparison = """? - A
                        |  - a
                        |: - B
                        |  - b
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[Map[List[VCChar], List[VCChar]]](yaml))
  }