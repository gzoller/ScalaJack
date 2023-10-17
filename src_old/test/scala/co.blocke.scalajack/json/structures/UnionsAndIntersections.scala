package co.blocke.scalajack
package json.structures

import co.blocke.scala_reflection._
import scala.math.BigDecimal
import java.util.UUID
import TestUtil._
import munit._
import munit.internal.console
import co.blocke.scalajack.json.JSON


class UnionsAndIntersections() extends FunSuite:

  val sj = co.blocke.scalajack.ScalaJack()
  
  test("Simple union type") {
    describe("-----------------------------------\n:  Union and Intersection Tests  :\n----------------------------------",Console.BLUE)

    // Right
    val inst = Multi2(List("a","b","c"))
    val js = sj.render(inst)
    assertEquals(js, """{"one":["a","b","c"]}""".asInstanceOf[JSON])
    assertEquals(sj.read[Multi2](js), inst)

    // Left
    val inst2 = Multi2(List(true,false))
    val js2 = sj.render(inst2)
    assertEquals(js2, """{"one":[true,false]}""".asInstanceOf[JSON])
    assertEquals(sj.read[Multi2](js2), inst2)

    // Failure
    val js3 = """{"one":12.34}""".asInstanceOf[JSON]
    val msg = """Failed to read any values for union type
    |{"one":12.34}
    |------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[Multi2](js3)
    } 
  }

  test("3-way union type") {
    // Position 1
    val inst = Multi3(List("a","b","c"))
    val js = sj.render(inst)
    assertEquals(js, """{"one":["a","b","c"]}""".asInstanceOf[JSON])
    assertEquals(sj.read[Multi3](js), inst)

    // Position 2
    val inst2 = Multi3(List(1,2,3))
    val js2 = sj.render(inst2)
    assertEquals(js2, """{"one":[1,2,3]}""".asInstanceOf[JSON])
    assertEquals(sj.read[Multi3](js2), inst2)

    // Position 3
    val inst3 = Multi3(false)
    val js3 = sj.render(inst3)
    assertEquals(js3, """{"one":false}""".asInstanceOf[JSON])
    assertEquals(sj.read[Multi3](js3), inst3)

    // Failure
    val js4 = """{"one":12.34}""".asInstanceOf[JSON]
    val msg = """Failed to read any values for union type
    |{"one":12.34}
    |------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[Multi3](js4)
    } 
  }

  test("4-way union type") {
    // Position 1
    val inst = Multi4(List("a","b","c"))
    val js = sj.render(inst)
    assertEquals(js, """{"one":["a","b","c"]}""".asInstanceOf[JSON])
    assertEquals(sj.read[Multi4](js), inst)

    // Position 2
    val inst2 = Multi4(List(1,2,3))
    val js2 = sj.render(inst2)
    assertEquals(js2, """{"one":[1,2,3]}""".asInstanceOf[JSON])
    assertEquals(sj.read[Multi4](js2), inst2)

    // Position 3
    val inst3 = Multi4(false)
    val js3 = sj.render(inst3)
    assertEquals(js3, """{"one":false}""".asInstanceOf[JSON])
    assertEquals(sj.read[Multi4](js3), inst3)

    // Position 4
    val inst4 = Multi4(Person("Bob",34))
    val js4 = sj.render(inst4)
    assertEquals(js4, """{"one":{"name":"Bob","age":34}}""".asInstanceOf[JSON])
    assertEquals(sj.read[Multi4](js4), inst4)

    // Failure
    val js5 = """{"one":12.34}""".asInstanceOf[JSON]
    val msg = """Failed to read any values for union type
    |{"one":12.34}
    |------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[Multi4](js5)
    } 
  }

  test("Intersection type") {
    val inst = IntersectionHolder( InterImpl(5,true,'Z') )
    val js = sj.render(inst)
    assertEquals(js, """{"a":{"_hint":"co.blocke.scalajack.json.structures.InterImpl","a":5,"b":true,"c":"Z"}}""".asInstanceOf[JSON])
    assertEquals(inst, sj.read[IntersectionHolder](js))
  }
