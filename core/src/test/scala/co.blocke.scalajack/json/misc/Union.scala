package co.blocke.scalajack
package json
package misc

import model._

import co.blocke.scalajack.typeadapter.UnionTypeAdapterFactory
import org.scalatest.Matchers
import org.scalatest.funspec.AnyFunSpec

case class Person(name: String, age: Int)
case class Multi2(one: Union2[List[String], List[Int]])
case class Multi3(one: Union3[List[String], List[Int], Boolean])
case class Multi4(one: Union4[List[String], List[Int], Boolean, Person])

class Union extends AnyFunSpec with Matchers {

  val sj = ScalaJack().withAdapters(UnionTypeAdapterFactory)

  describe("---------------------\n:  Union Tests  :\n---------------------") {
    it("Union2") {
      val mk2 = Multi2(Union2(Some(List("a", "b", "c")), None))
      val js = sj.render(mk2)
      js should be("""{"one":["a","b","c"]}""")
      val i = sj.read[Multi2](js)
      i should be(mk2)
      i.one.unpack should be(List("a", "b", "c"))
      i.one._unpack should be((List("a", "b", "c"), 0))

      val mk2_2 = Multi2(Union2(None, Some(List(1, 2, 3))))
      val js2 = sj.render(mk2_2)
      js2 should be("""{"one":[1,2,3]}""")
      val i2 = sj.read[Multi2](js2)
      i2 should be(mk2_2)
      i2.one.unpack should be(List(1, 2, 3))
      i2.one._unpack should be((List(1, 2, 3), 1))

      val mk2_3: Multi2 = Multi2(null)
      val jsx = sj.render(mk2_3)
      jsx should be("""{"one":null}""")
      sj.read[Multi2](jsx) should be(mk2_3)

      val js3 = """{"one":true}"""
      val msg =
        """[$.one]: Failed to read any kind of a Union value
          |{"one":true}
          |----------^""".stripMargin
      the[ReadMalformedError] thrownBy sj.read[Multi2](js3) should have message msg
    }
    it("Union3") {
      val mk3_0 = Multi3(null)
      val jsx = sj.render(mk3_0)
      jsx should be("""{"one":null}""")
      sj.read[Multi3](jsx) should be(mk3_0)

      val mk3 = Multi3(Union3(Some(List("a", "b", "c")), None, None))
      val js = sj.render(mk3)
      js should be("""{"one":["a","b","c"]}""")
      val i = sj.read[Multi3](js)
      i should be(mk3)
      i.one.unpack should be(List("a", "b", "c"))
      i.one._unpack should be((List("a", "b", "c"), 0))

      val mk3_2 = Multi3(Union3(None, Some(List(1, 2, 3)), None))
      val js2 = sj.render(mk3_2)
      js2 should be("""{"one":[1,2,3]}""")
      val i2 = sj.read[Multi3](js2)
      i2 should be(mk3_2)
      i2.one.unpack should be(List(1, 2, 3))
      i2.one._unpack should be((List(1, 2, 3), 1))

      val mk3_3 = Multi3(Union3(None, None, Some(true)))
      val js3 = sj.render(mk3_3)
      js3 should be("""{"one":true}""")
      val i3 = sj.read[Multi3](js3)
      i3 should be(mk3_3)
      i3.one.unpack should equal(true)
      i3.one._unpack should be((true, 2))

      val js9 = """{"one":12.34}"""
      val msg =
        """[$.one]: Failed to read any kind of a Union value
          |{"one":12.34}
          |-----------^""".stripMargin
      the[ReadMalformedError] thrownBy sj.read[Multi3](js9) should have message msg
    }
    it("Union4") {
      val mk4_0 = Multi4(null)
      val jsx = sj.render(mk4_0)
      jsx should be("""{"one":null}""")
      sj.read[Multi4](jsx) should be(mk4_0)

      val mk4 = Multi4(Union4(Some(List("a", "b", "c")), None, None, None))
      val js = sj.render(mk4)
      js should be("""{"one":["a","b","c"]}""")
      val i = sj.read[Multi4](js)
      i should be(mk4)
      i.one.unpack should be(List("a", "b", "c"))
      i.one._unpack should be((List("a", "b", "c"), 0))

      val mk4_2 = Multi4(Union4(None, Some(List(1, 2, 3)), None, None))
      val js2 = sj.render(mk4_2)
      js2 should be("""{"one":[1,2,3]}""")
      val i2 = sj.read[Multi4](js2)
      i2 should be(mk4_2)
      i2.one.unpack should be(List(1, 2, 3))
      i2.one._unpack should be((List(1, 2, 3), 1))

      val mk4_3 = Multi4(Union4(None, None, Some(true), None))
      val js3 = sj.render(mk4_3)
      js3 should be("""{"one":true}""")
      val i3 = sj.read[Multi4](js3)
      i3 should be(mk4_3)
      i3.one.unpack should equal(true)
      i3.one._unpack should be((true, 2))

      val mk4_4 = Multi4(Union4(None, None, None, Some(Person("Greg", 32))))
      val js4 = sj.render(mk4_4)
      js4 should be("""{"one":{"name":"Greg","age":32}}""")
      val i4 = sj.read[Multi4](js4)
      i4 should be(mk4_4)
      i4.one.unpack should equal(Person("Greg", 32))
      i4.one._unpack should be((Person("Greg", 32), 3))

      val js9 = """{"one":12.34}"""
      val msg =
        """[$.one]: Failed to read any kind of a Union value
          |{"one":12.34}
          |-----------^""".stripMargin
      the[ReadMalformedError] thrownBy sj.read[Multi3](js9) should have message msg
    }
  }
}